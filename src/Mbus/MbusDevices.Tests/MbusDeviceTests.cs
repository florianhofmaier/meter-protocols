using System.Collections.Concurrent;
using System.Threading.Channels;
using Shouldly;

namespace Metering.Mbus.MbusDevices.Tests;

public class MbusDeviceTests
{
    private static readonly byte[] Identity = [0x78, 0x56, 0x34, 0x12, 0xE6, 0x1E, 0x3C, 0x07];

    private static MbusDevice TestDevice =>
        MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);

    [Fact]
    public void Create_AcceptsValidIdentity() => TestDevice.ShouldNotBeNull();

    [Theory]
    [InlineData(1, "AAA", 0, MbusDeviceType.Other, "0100000021040000")]
    [InlineData(99999999, "ZZZ", 254, MbusDeviceType.WaterMeter, "999999995A6BFE07")]
    public async Task Create_EncodesBoundaryIdentities(
        int id, string manufacturer, int version, MbusDeviceType type, string expectedIdentity)
    {
        var device = MbusDevice.Create(id, manufacturer, version, type);
        (await Run(device, Request(5))).ShouldBe(
            Response([], identity: Convert.FromHexString(expectedIdentity)));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(-1)]
    [InlineData(100_000_000)]
    public void Create_RejectsInvalidIdentificationNumber(int id) =>
        Should.Throw<ArgumentException>(() => MbusDevice.Create(id, "GWF", 60, MbusDeviceType.WaterMeter));

    [Theory]
    [InlineData(null)]
    [InlineData("")]
    [InlineData("INVALID")]
    [InlineData("gwf")]
    [InlineData("A1C")]
    public void Create_RejectsInvalidManufacturer(string? manufacturer) =>
        Should.Throw<ArgumentException>(() =>
            MbusDevice.Create(12345678, manufacturer!, 60, MbusDeviceType.WaterMeter));

    [Theory]
    [InlineData(-1)]
    [InlineData(255)]
    [InlineData(256)]
    [InlineData(300)]
    public void Create_RejectsVersionWithoutTruncatingIt(int version) =>
        Should.Throw<ArgumentException>(() =>
            MbusDevice.Create(12345678, "GWF", version, MbusDeviceType.WaterMeter));

    [Fact]
    public void Create_RejectsUndefinedDeviceType() =>
        Should.Throw<ArgumentOutOfRangeException>(() =>
            MbusDevice.Create(12345678, "GWF", 60, (MbusDeviceType)256));

    [Fact]
    public void UpdateUserData_RejectsNullCallbacks()
    {
        Should.Throw<ArgumentNullException>(() => TestDevice.UpdateUserData((Action<MbusUserDataBuilder>)null!));
        Should.Throw<ArgumentNullException>(() =>
            TestDevice.UpdateUserData(builder => builder.AddInt32(1, null!)));
        Should.Throw<ArgumentException>(() =>
            TestDevice.UpdateUserData(builder => builder.AddInt32(1, _ => null!)));
    }

    // EN 13757-3:2025, Tables 4/10/15: DIF=04; primary volume VIFs and FB extension.
    [Theory]
    [InlineData(-6, "10")]
    [InlineData(-5, "11")]
    [InlineData(-4, "12")]
    [InlineData(-3, "13")]
    [InlineData(-2, "14")]
    [InlineData(-1, "15")]
    [InlineData(0, "16")]
    [InlineData(1, "17")]
    [InlineData(2, "FB10")]
    [InlineData(3, "FB11")]
    public async Task Builder_EncodesEveryVolumeScale(int exponent, string vif)
    {
        var output = await Run(Request(5), builder =>
            builder.AddInt32(1234, record => Volume(record, exponent)));

        output.ShouldBe(Response(Convert.FromHexString("04" + vif + "D2040000")));
    }

    [Fact]
    public async Task Builder_EncodesFabricationNumber()
    {
        var output = await Run(Request(5), builder =>
            builder.AddInt32(1234, record => record.AsFabricationNumber()));
        output.ShouldBe(Response(Convert.FromHexString("0478D2040000")));
    }

    [Theory]
    [InlineData(MbusFunctionField.InstValue, "04")]
    [InlineData(MbusFunctionField.MaxValue, "14")]
    [InlineData(MbusFunctionField.MinValue, "24")]
    [InlineData(MbusFunctionField.ValueInErrorState, "34")]
    public async Task Builder_EncodesFunctionFieldAndSignedValue(MbusFunctionField function, string dif)
    {
        var output = await Run(Request(5), builder =>
            builder.AddInt32(-1234, record => record.AsCubicMetersExpMinus3().WithFunctionField(function)));

        output.ShouldBe(Response(Convert.FromHexString(dif + "132EFBFFFF")));
    }

    [Fact]
    public async Task Builder_EncodesStorageTariffAndSubunitAcrossMultipleDifes()
    {
        // EN 13757-3:2025, Table 8: storage=35, tariff=5, subunit=3, maximum value.
        var output = await Run(Request(5), builder =>
            builder.AddInt32(1234, record => record.AsCubicMetersExpMinus3()
                .WithStorageNumber(35).WithTariff(5).WithSubUnit(3)
                .WithFunctionField(MbusFunctionField.MaxValue)));

        output.ShouldBe(Response(Convert.FromHexString("D4D15113D2040000")));
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(1024)]
    public void Builder_RejectsInvalidSubunit(int value) =>
        Should.Throw<ArgumentException>(() => TestDevice.UpdateUserData(builder =>
            builder.AddInt32(1, record => record.AsCubicMetersExp0().WithSubUnit(value))));

    [Theory]
    [InlineData(-1)]
    [InlineData(1048576)]
    public void Builder_RejectsInvalidTariff(int value) =>
        Should.Throw<ArgumentException>(() => TestDevice.UpdateUserData(builder =>
            builder.AddInt32(1, record => record.AsCubicMetersExp0().WithTariff(value))));

    [Fact]
    public void Builder_RejectsNegativeStorageAndUndefinedFunction()
    {
        Should.Throw<ArgumentException>(() => TestDevice.UpdateUserData(builder =>
            builder.AddInt32(1, record => record.AsCubicMetersExp0().WithStorageNumber(-1))));
        Should.Throw<ArgumentOutOfRangeException>(() => TestDevice.UpdateUserData(builder =>
            builder.AddInt32(1, record => record.AsCubicMetersExp0()
                .WithFunctionField((MbusFunctionField)4))));
    }

    [Fact]
    public async Task UpdateUserData_ReplacesRecordsAndPreservesTheirOrder()
    {
        var device = TestDevice;
        device.UpdateUserData(builder => builder.AddInt32(1, record => record.AsCubicMetersExp0()));
        device.UpdateUserData(builder => builder.AddInt32(1234, record => record.AsCubicMetersExpMinus3())
            .AddInt32(2, record => record.AsCubicMetersExp0()));

        (await Run(device, Request(5))).ShouldBe(Response(Convert.FromHexString("0413D2040000041602000000")));
        device.UpdateUserData(_ => { });
        (await Run(device, Request(5))).ShouldBe(Response([]));
    }

    [Fact]
    public async Task UpdateUserData_FailedUpdatePreservesThePublishedRecords()
    {
        var device = TestDevice;
        device.UpdateUserData(builder => builder.AddInt32(1234, record => record.AsCubicMetersExpMinus3()));
        Should.Throw<InvalidOperationException>(() => device.UpdateUserData(builder =>
        {
            builder.AddInt32(99, record => record.AsCubicMetersExp0());
            throw new InvalidOperationException("Cannot finish update");
        }));

        (await Run(device, Request(5))).ShouldBe(Response(Convert.FromHexString("0413D2040000")));
    }

    [Fact]
    public async Task UpdateUserData_CapturedBuilderCannotChangePublishedRecords()
    {
        var device = TestDevice;
        MbusUserDataBuilder? captured = null;
        device.UpdateUserData(builder =>
        {
            captured = builder;
            builder.AddInt32(1234, record => record.AsCubicMetersExpMinus3());
        });
        captured!.AddInt32(99, record => record.AsCubicMetersExp0());

        (await Run(device, Request(5))).ShouldBe(Response(Convert.FromHexString("0413D2040000")));
    }

    [Theory]
    [InlineData("0213D204")]
    [InlineData("0C1378563412")]
    [InlineData("0D13E20102")]
    public async Task UpdateUserData_AcceptsEncodedRecords(string records)
    {
        var device = TestDevice;
        var bytes = Convert.FromHexString(records);
        device.UpdateUserData(new CustomBuilder(() => bytes));
        (await Run(device, Request(5))).ShouldBe(Response(bytes));
    }

    [Fact]
    public async Task UpdateUserData_OwnsBinaryAndManufacturerBytes()
    {
        var device = TestDevice;
        var bytes = Convert.FromHexString("0D13E201020FAABB");
        device.UpdateUserData(new CustomBuilder(() => bytes));
        Array.Fill(bytes, (byte)0);

        (await Run(device, Request(5))).ShouldBe(
            Response(Convert.FromHexString("0D13E201020FAABB")));
    }

    [Theory]
    [InlineData("04")]
    [InlineData("0413D2")]
    [InlineData("047E00000000")]
    [InlineData("1F")]
    public async Task UpdateUserData_InvalidEncodedDataPreservesPreviousData(string invalid)
    {
        var device = TestDevice;
        device.UpdateUserData(new CustomBuilder(() => Convert.FromHexString("0413D2040000")));
        var error = Should.Throw<ArgumentException>(() =>
            device.UpdateUserData(new CustomBuilder(() => Convert.FromHexString(invalid))));
        error.ParamName.ShouldBe("builder");
        (await Run(device, Request(5))).ShouldBe(Response(Convert.FromHexString("0413D2040000")));
    }

    [Fact]
    public async Task UpdateUserData_EmptyEncodedDataClearsTheRecords()
    {
        var device = TestDevice;
        device.UpdateUserData(new CustomBuilder(() => Convert.FromHexString("0413D2040000")));
        device.UpdateUserData(new CustomBuilder(() => []));
        (await Run(device, Request(5))).ShouldBe(Response([]));
    }

    [Fact]
    public void UpdateUserData_RejectsNullCustomBuilders()
    {
        Should.Throw<ArgumentNullException>(() =>
            TestDevice.UpdateUserData((IMbusUserDataBuilder)null!)).ParamName.ShouldBe("builder");
    }

    [Fact]
    public async Task UpdateUserData_RejectsNullBuilderOutputWithoutClearingTheData()
    {
        var device = TestDevice;
        device.UpdateUserData(data => data.AddInt32(1234, r => r.AsCubicMetersExpMinus3()));
        Should.Throw<ArgumentException>(() =>
            device.UpdateUserData(new CustomBuilder(() => null!))).ParamName.ShouldBe("builder");
        (await Run(device, Request(5))).ShouldBe(Response(Convert.FromHexString("0413D2040000")));
    }

    [Fact]
    public async Task UpdateUserData_BuildFailuresLeaveTheRunningDeviceUsable()
    {
        var device = TestDevice;
        byte[] current = Convert.FromHexString("0413D2040000");
        var custom = new CustomBuilder(() => current);
        device.UpdateUserData(custom);
        custom.BuildCalls.ShouldBe(1);
        using var stream = new DuplexStream();
        using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
        var running = device.RunAsync(stream, 5, cts.Token);
        stream.Enqueue(Request(5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(current));
        var failure = new InvalidOperationException("Cannot build new data");
        Should.Throw<InvalidOperationException>(() =>
            device.UpdateUserData(new CustomBuilder(() => throw failure))).ShouldBeSameAs(failure);
        stream.Enqueue(ShortFrame(0x7B, 5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(current, access: 1));
        current = Convert.FromHexString("0413E7030000");
        stream.Enqueue(Request(5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(Convert.FromHexString("0413D2040000"), access: 2));
        custom.BuildCalls.ShouldBe(1);
        device.UpdateUserData(custom);
        custom.BuildCalls.ShouldBe(2);
        stream.Enqueue(ShortFrame(0x7B, 5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(current, access: 3));
        stream.CompleteInput();
        await running;
    }

    [Fact]
    public async Task UpdateUserData_UnsendableRecordsDoNotReplaceValidData()
    {
        var device = TestDevice;
        device.UpdateUserData(data => data.AddInt32(1234, r => r.AsCubicMetersExpMinus3()));
        // A valid text VIF with 255 characters already exceeds one wired response.
        var oversized = new byte[] { 4, 0x7C, 255 }
            .Concat(Enumerable.Repeat((byte)'x', 255)).Concat(new byte[4]).ToArray();
        Should.Throw<ArgumentException>(() =>
            device.UpdateUserData(new CustomBuilder(() => oversized)));
        (await Run(device, Request(5))).ShouldBe(Response(Convert.FromHexString("0413D2040000")));
    }

    [Theory]
    [InlineData(0)]
    [InlineData(1)]
    [InlineData(250)]
    public async Task RunAsync_UsesConfiguredPrimaryAddress(byte address)
    {
        var device = MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);
        (await Run(device, Request(address), address)).ShouldBe(Response([], address));
    }

    [Theory]
    [InlineData(251)]
    [InlineData(252)]
    [InlineData(253)]
    [InlineData(254)]
    [InlineData(255)]
    public void RunAsync_RejectsServiceAddressesAsDevicePrimaryAddress(byte address)
    {
        using var stream = new DuplexStream([]);
        Should.Throw<ArgumentOutOfRangeException>(() =>
            TestDevice.RunAsync(stream, address, CancellationToken.None));
    }

    [Fact]
    public async Task RunAsync_IgnoresOtherAddressesAndUnselectedSecondaryAddress()
    {
        (await Run(Request(6))).ShouldBeEmpty();
        (await Run(Request(253))).ShouldBeEmpty();
    }

    [Theory]
    [InlineData(0x53)]
    [InlineData(0x73)]
    public async Task RunAsync_SelectsTheDeviceAndAnswersAtSecondaryAddress(byte control)
    {
        // EN 13757-7:2018, 8.4/Table 44: ACK followed by RSP-UD with the device's own primary address.
        var input = Selection(Identity, control).Concat(Request(253)).ToArray();
        (await Run(input)).ShouldBe(new byte[] { 0xE5 }.Concat(Response([])).ToArray());
    }

    [Fact]
    public async Task RunAsync_HandlesWildcardAndNonmatchingSelection()
    {
        var wildcard = Enumerable.Repeat((byte)0xFF, 8).ToArray();
        (await Run(Selection(wildcard).Concat(Request(253)).ToArray()))
            .ShouldBe(new byte[] { 0xE5 }.Concat(Response([])).ToArray());
        var other = Identity.ToArray();
        other[0] = 0x79;
        (await Run(Selection(other).Concat(Request(253)).ToArray())).ShouldBeEmpty();
    }

    [Fact]
    public async Task RunAsync_DeselectsOnSecondaryResetButKeepsSelectionOnPrimaryReset()
    {
        var prefix = Selection(Identity);
        var secondary = prefix.Concat(ShortFrame(0x40, 253)).Concat(Request(253)).ToArray();
        (await Run(secondary)).ShouldBe(new byte[] { 0xE5, 0xE5 });
        var primary = prefix.Concat(ShortFrame(0x40, 5)).Concat(Request(253)).ToArray();
        (await Run(primary)).ShouldBe(new byte[] { 0xE5, 0xE5 }.Concat(Response([])).ToArray());
    }

    [Fact]
    public async Task RunAsync_SkipsCorruptFrameAndProcessesFragmentedValidRequest()
    {
        var corrupt = Request(5);
        corrupt[3] = 0;
        (await Run(corrupt.Concat(Request(5)).ToArray())).ShouldBe(Response([]));
    }

    [Fact]
    public async Task RunAsync_AcknowledgesClassOneRequestWithoutAlarms()
    {
        var input = new byte[] { 0xE5 }.Concat(ShortFrame(0x5A, 5)).ToArray();
        (await Run(input)).ShouldBe(new byte[] { 0xE5 });
    }

    [Fact]
    public async Task RunAsync_RejectsMalformedResetAndSelectionEnvelopes()
    {
        // IEC 60870-5-2, 5.1.2/Table 1, pp. 22-23; EN 13757-7, Table 44.
        (await Run(ShortFrame(0x50, 5))).ShouldBeEmpty();
        (await Run(ShortFrame(0x60, 5))).ShouldBeEmpty();
        (await Run(Selection(Identity, 0x43).Concat(Request(253)).ToArray())).ShouldBeEmpty();
        var wrongAddress = LongFrame(new byte[] { 0x53, 5, 0x52 }.Concat(Identity).ToArray());
        (await Run(wrongAddress.Concat(Request(253)).ToArray())).ShouldBeEmpty();
    }

    [Fact]
    public async Task RunAsync_RejectsTruncatedInput()
    {
        using var stream = new DuplexStream([0x10, 0x5B]);
        await Should.ThrowAsync<InvalidDataException>(() =>
            TestDevice.RunAsync(stream, 5, CancellationToken.None));
    }

    [Fact]
    public async Task UpdateUserData_WhileRunningIsVisibleToTheNextRequest()
    {
        var device = TestDevice;
        using var stream = new DuplexStream();
        using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
        var running = device.RunAsync(stream, 5, cts.Token);
        stream.Enqueue(Request(5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response([]));
        device.UpdateUserData(builder => builder.AddInt32(1234, record => record.AsCubicMetersExpMinus3()));
        stream.Enqueue(ShortFrame(0x7B, 5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(Convert.FromHexString("0413D2040000"), access: 1));
        stream.CompleteInput();
        await running;
    }

    [Fact]
    public async Task RunAsync_HonorsCancellationDuringRead()
    {
        using var stream = new DuplexStream();
        using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
        var running = TestDevice.RunAsync(stream, 5, cts.Token);
        await stream.ReadStarted.Task.WaitAsync(cts.Token);
        await cts.CancelAsync();
        await Should.ThrowAsync<OperationCanceledException>(() => running);
    }

    [Fact]
    public async Task RunAsync_FreezesMultiTelegramDataAndRepeatsBlocks()
    {
        var device = TestDevice;
        var builder = new MbusUserDataBuilder();
        for (var i = 0; i < 45; i++) builder.AddInt32(i, r => r.AsCubicMetersExpMinus3());
        var original = builder.Build();
        device.UpdateUserData(new CustomBuilder(() => original));
        var first = Convert.FromHexString("0DFDFD00E3000000")
            .Concat(original.Take(38 * 6)).Append((byte)0x1F).ToArray();
        var last = Convert.FromHexString("0DFDFD00E3010000")
            .Concat(original.Skip(38 * 6)).ToArray();
        using var stream = new DuplexStream();
        using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
        var running = device.RunAsync(stream, 5, cts.Token);
        stream.Enqueue(Request(5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(first));
        device.UpdateUserData(new MbusUserDataBuilder().AddInt32(999, r => r.AsCubicMetersExpMinus3()));
        stream.Enqueue(Request(5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(first, access: 1));
        stream.Enqueue(ShortFrame(0x7B, 5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(last, access: 2));
        stream.Enqueue(ShortFrame(0x7B, 5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(last, access: 3));
        stream.Enqueue(Request(5));
        (await stream.NextWrite(cts.Token)).ShouldBe(Response(Convert.FromHexString("0413E7030000"), access: 4));
        stream.CompleteInput();
        await running;
    }

    [Fact]
    public async Task RunAsync_SupportsSelectedApplicationAndReturnsToStandardResponse()
    {
        var command = LongFrame(new byte[] { 0x53, 5, 0x54 });
        var reset = LongFrame(new byte[] { 0x53, 5, 0x50 });
        var selected = LongFrame(new byte[] { 8, 5, 0x68 }.Concat(Identity)
            .Concat(new byte[] { 0, 0, 0, 0, 0, 0, 0 }).ToArray());
        var input = command.Concat(Request(5)).Concat(reset).Concat(ShortFrame(0x7B, 5)).ToArray();
        (await Run(input)).ShouldBe(new byte[] { 0xE5 }.Concat(selected)
            .Append((byte)0xE5).Concat(Response([], access: 1)).ToArray());
    }

    [Fact]
    public async Task RunAsync_ImmediatelyReadsSelectedApplicationAcrossMultipleBlocks()
    {
        var device = TestDevice;
        var builder = new MbusUserDataBuilder();
        for (var i = 0; i < 45; i++) builder.AddInt32(i, r => r.AsCubicMetersExpMinus3());
        var records = builder.Build();
        device.UpdateUserData(new CustomBuilder(() => records));
        var first = Convert.FromHexString("0DFDFD00E3000000")
            .Concat(records.Take(38 * 6)).Append((byte)0x1F).ToArray();
        var last = Convert.FromHexString("0DFDFD00E3010000")
            .Concat(records.Skip(38 * 6)).ToArray();
        byte[] Selected(byte[] payload, byte block, byte access) =>
            LongFrame(new byte[] { 8, 5, 0x68 }.Concat(Identity)
                .Concat(new byte[] { access, 0, 0, 0, block, 0, 0 }).Concat(payload).ToArray());
        var input = LongFrame(new byte[] { 0x43, 5, 0x54 }).Concat(ShortFrame(0x7B, 5)).ToArray();
        (await Run(device, input)).ShouldBe(Selected(first, 0, 0).Concat(Selected(last, 1, 1)).ToArray());
    }

    [Fact]
    public async Task RunAsync_RejectsUnsupportedSendUd2OnlyWhenAddressed()
    {
        (await Run(LongFrame(new byte[] { 0x43, 5, 0x51 }))).ShouldBe(new byte[] { 0xA2 });
        (await Run(LongFrame(new byte[] { 0x43, 6, 0x51 }))).ShouldBeEmpty();
        (await Run(LongFrame(new byte[] { 0x43, 255, 0x51 }))).ShouldBeEmpty();
    }

    private static IMbusRecordWithUnitBuilder Volume(IMbusRecordWithValueBuilder record, int exponent) =>
        exponent switch
        {
            -6 => record.AsCubicMetersExpMinus6(),
            -5 => record.AsCubicMetersExpMinus5(),
            -4 => record.AsCubicMetersExpMinus4(),
            -3 => record.AsCubicMetersExpMinus3(),
            -2 => record.AsCubicMetersExpMinus2(),
            -1 => record.AsCubicMetersExpMinus1(),
            0 => record.AsCubicMetersExp0(),
            1 => record.AsCubicMetersExp1(),
            2 => record.AsCubicMetersExp2(),
            3 => record.AsCubicMetersExp3(),
            _ => throw new ArgumentOutOfRangeException(nameof(exponent))
        };

    private static Task<byte[]> Run(byte[] input, Action<MbusUserDataBuilder>? update = null)
    {
        var device = TestDevice;
        if (update is not null) device.UpdateUserData(update);
        return Run(device, input);
    }

    private static async Task<byte[]> Run(MbusDevice device, byte[] input, byte primaryAddress = 5)
    {
        using var stream = new DuplexStream(input);
        using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(10));
        await device.RunAsync(stream, primaryAddress, cts.Token);
        return stream.Output;
    }

    private static byte[] Request(byte address) => ShortFrame(0x5B, address);

    private static byte[] ShortFrame(byte control, byte address) =>
        [0x10, control, address, unchecked((byte)(control + address)), 0x16];

    private static byte[] Selection(byte[] identity, byte control = 0x53) =>
        LongFrame(new byte[] { control, 0xFD, 0x52 }.Concat(identity).ToArray());

    private static byte[] Response(byte[] records, byte address = 5, byte[]? identity = null, byte access = 0) =>
        LongFrame(new byte[] { 0x08, address, 0x72 }.Concat(identity ?? Identity)
            .Concat(new byte[] { access, 0, 0, 0 }).Concat(records).ToArray());

    private static byte[] LongFrame(byte[] body) =>
        new byte[] { 0x68, (byte)body.Length, (byte)body.Length, 0x68 }
            .Concat(body).Concat(new byte[] { unchecked((byte)body.Sum(b => b)), 0x16 }).ToArray();

    private sealed class CustomBuilder(Func<byte[]> build) : IMbusUserDataBuilder
    {
        public int BuildCalls { get; private set; }

        public byte[] Build()
        {
            BuildCalls++;
            return build();
        }
    }

    private sealed class DuplexStream : Stream
    {
        private readonly Channel<byte[]> _input = Channel.CreateUnbounded<byte[]>();
        private readonly Channel<byte[]> _output = Channel.CreateUnbounded<byte[]>();
        private readonly ConcurrentQueue<byte[]> _writes = new();
        private byte[]? _pending;
        private int _offset;

        internal DuplexStream() { }

        internal DuplexStream(byte[] input)
        {
            foreach (var value in input) Enqueue([value]);
            CompleteInput();
        }

        internal TaskCompletionSource ReadStarted { get; } =
            new(TaskCreationOptions.RunContinuationsAsynchronously);
        internal byte[] Output => _writes.SelectMany(bytes => bytes).ToArray();
        internal void Enqueue(byte[] bytes) => _input.Writer.TryWrite(bytes);
        internal void CompleteInput() => _input.Writer.TryComplete();
        internal ValueTask<byte[]> NextWrite(CancellationToken ct) => _output.Reader.ReadAsync(ct);

        public override async ValueTask<int> ReadAsync(
            Memory<byte> buffer, CancellationToken cancellationToken = default)
        {
            ReadStarted.TrySetResult();
            if (_pending is null)
            {
                if (!await _input.Reader.WaitToReadAsync(cancellationToken)) return 0;
                _pending = await _input.Reader.ReadAsync(cancellationToken);
                _offset = 0;
            }
            var count = Math.Min(buffer.Length, _pending.Length - _offset);
            _pending.AsMemory(_offset, count).CopyTo(buffer);
            _offset += count;
            if (_offset == _pending.Length) _pending = null;
            return count;
        }

        public override ValueTask WriteAsync(
            ReadOnlyMemory<byte> buffer, CancellationToken cancellationToken = default)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var bytes = buffer.ToArray();
            _writes.Enqueue(bytes);
            _output.Writer.TryWrite(bytes);
            return ValueTask.CompletedTask;
        }

        public override bool CanRead => true;
        public override bool CanWrite => true;
        public override bool CanSeek => false;
        public override long Length => throw new NotSupportedException();
        public override long Position
        {
            get => throw new NotSupportedException();
            set => throw new NotSupportedException();
        }
        public override void Flush() { }
        public override int Read(byte[] buffer, int offset, int count) => throw new NotSupportedException();
        public override void Write(byte[] buffer, int offset, int count) => throw new NotSupportedException();
        public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();
        public override void SetLength(long value) => throw new NotSupportedException();
    }
}
