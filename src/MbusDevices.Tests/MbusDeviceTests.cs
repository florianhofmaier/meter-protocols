using Shouldly;

namespace Metering.Mbus.MbusDevices.Tests;

public class MbusDeviceTests
{
    private static MbusDevice TestMbusDevice =>
        MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);

    [Fact]
    public void Create_WithValidParameters_ShouldReturnDevice()
    {
        // Act
        var device = TestMbusDevice;

        // Assert
        device.ShouldNotBeNull();
    }

    [Fact]
    public void Create_WithInvalidManufacturer_ShouldThrowArgumentException()
    {
        // Act
        var exception = Record.Exception(() =>
            MbusDevice.Create(12345678, "INVALID", 60, MbusDeviceType.WaterMeter));

        // Assert
        exception.ShouldNotBeNull();
        exception.ShouldBeOfType<ArgumentException>();
    }

    [Fact]
    public void Create_WithNegativeIdNumber_ShouldThrowArgumentException()
    {
        // Act
        var exception = Record.Exception(() =>
            MbusDevice.Create(-1, "GWF", 60, MbusDeviceType.WaterMeter));

        // Assert
        exception.ShouldNotBeNull();
        exception.ShouldBeOfType<ArgumentException>();
    }

    [Fact]
    public void Create_WithIdNumberTooLarge_ShouldThrowArgumentException()
    {
        // Act
        var exception = Record.Exception(() =>
            MbusDevice.Create(100_000_000, "GWF", 60, MbusDeviceType.WaterMeter));

        // Assert
        exception.ShouldNotBeNull();
        exception.ShouldBeOfType<ArgumentException>();
    }

    [Fact]
    public void UpdateUserData_WithEmptyAction_ShouldSucceed()
    {
        // Arrange
        var device = TestMbusDevice;

        // Act
        var exception = Record.Exception(() => device.UpdateUserData(_ => { }));

        // Assert
        exception.ShouldBeNull();
    }

    [Fact]
    public void UpdateUserData_WithSingleRecord_ShouldSucceed()
    {
        // Arrange
        var device = TestMbusDevice;

        // Act
        var exception = Record.Exception((Action)(() =>
            device.UpdateUserData(builder =>
                builder.AddInt32(1234, rb =>
                    rb.AsCubicMetersExpMinus3()))));

        // Assert
        exception.ShouldBeNull();
    }

    [Fact]
    public void UpdateUserData_WithMultipleRecords_ShouldSucceed()
    {
        // Arrange
        var device = TestMbusDevice;

        // Act
        var exception = Record.Exception((Action)(() =>
            device.UpdateUserData(builder =>
            {
                builder.AddInt32(1234, rb => rb.AsCubicMetersExpMinus3()); // Liters
                builder.AddInt32(5678, rb => rb.AsCubicMetersExp0()); // Cubic meters
            })));

        // Assert
        exception.ShouldBeNull();
    }

    [Fact]
    public void UpdateUserData_WithAllRecordOptions_ShouldSucceed()
    {
        // Arrange
        var device = TestMbusDevice;

        // Act
        var exception = Record.Exception((Action)(() =>
            device.UpdateUserData(builder =>
                builder.AddInt32(1234, rb =>
                    rb.AsCubicMetersExpMinus3()
                        .WithFunctionField(MbusFunctionField.InstValue)
                        .WithStorageNumber(0)
                        .WithSubUnit(0)
                        .WithTariff(0)))));

        // Assert
        exception.ShouldBeNull();
    }

    [Fact]
    public void UpdateUserData_CalledMultipleTimes_ShouldSucceed()
    {
        // Arrange
        var device = TestMbusDevice;
        device.UpdateUserData(builder =>
            builder.AddInt32(1000, rb => rb.AsCubicMetersExpMinus3())); // Liters

        // Act
        var exception = Record.Exception((Action)(() =>
            device.UpdateUserData(builder =>
                builder.AddInt32(2000, rb => rb.AsCubicMetersExpMinus3())))); // Liters

        // Assert
        exception.ShouldBeNull();
    }

    [Fact]
    public async Task RunAsync_WithReqUd2ToPrimaryAddress_ShouldWriteResponse()
    {
        // Arrange
        var reqUd2Frame = new byte[] { 0x10, 0x5B, 0x05, 0x60, 0x16 };
        using var stream = new MemoryStream();
        await stream.WriteAsync(reqUd2Frame);
        stream.Position = 0;

        var device = TestMbusDevice;
        device.UpdateUserData(builder =>
            builder.AddInt32(1234, rb => rb.AsCubicMetersExpMinus3())); // Liters

        using var cts = new CancellationTokenSource();

        // Act
        await device.RunAsync(stream, primaryAddress: 0x05, cts.Token);

        // Assert
        stream.Position = reqUd2Frame.Length;
        var responseLength = stream.Length - reqUd2Frame.Length;
        responseLength.ShouldBeGreaterThan(0);
    }

    [Fact]
    public async Task RunAsync_WithReqUd2ToDifferentAddress_ShouldNotWriteResponse()
    {
        // Arrange
        var reqUd2Frame = new byte[] { 0x10, 0x5B, 0x06, 0x61, 0x16 };
        using var stream = new MemoryStream();
        await stream.WriteAsync(reqUd2Frame);
        stream.Position = 0;

        var device = TestMbusDevice;
        device.UpdateUserData(_ => { });

        using var cts = new CancellationTokenSource();

        // Act
        await device.RunAsync(stream, primaryAddress: 0x05, cts.Token);

        // Assert
        stream.Position = reqUd2Frame.Length;
        var responseLength = stream.Length - reqUd2Frame.Length;
        responseLength.ShouldBe(0);
    }

    [Fact]
    public async Task RunAsync_WithDeviceSelection_ShouldSelectAndRespond()
    {
        // Arrange
        var selection = new byte[] { 0x78, 0x56, 0x34, 0x12, 0xE6, 0x1E, 0x3C, 0x07 };
        var selectionFrame = new byte[]
        {
            0x68, 0x0B, 0x0B, 0x68,
            0x53, 0xFD, 0x52,
        }
        .Concat(selection)
        .Concat(new byte[] { 0x00, 0x16 })
        .ToArray();

        byte checksum = 0;
        for (int i = 4; i <= 14; i++)
            checksum = (byte)((checksum + selectionFrame[i]) & 0xFF);
        selectionFrame[15] = checksum;

        var reqUd2ToFd = new byte[] { 0x10, 0x5B, 0xFD, 0x58, 0x16 };
        var fullFrame = selectionFrame.Concat(reqUd2ToFd).ToArray();

        using var stream = new MemoryStream();
        await stream.WriteAsync(fullFrame);
        stream.Position = 0;

        var device = TestMbusDevice;
        device.UpdateUserData(_ => { });

        using var cts = new CancellationTokenSource();

        // Act
        await device.RunAsync(stream, primaryAddress: 0x05, cts.Token);

        // Assert
        stream.Position = fullFrame.Length;
        var responseLength = stream.Length - fullFrame.Length;
        responseLength.ShouldBeGreaterThan(1); // Confirmation + Response
    }

    [Fact]
    public async Task UpdateUserData_WhileDeviceRunning_ShouldBeThreadSafe()
    {
        // Arrange
        var reqUd2Frame = new byte[] { 0x10, 0x5B, 0x05, 0x60, 0x16 };
        using var stream = new MemoryStream();
        await stream.WriteAsync(reqUd2Frame);
        stream.Position = 0;

        var device = TestMbusDevice;
        device.UpdateUserData(builder =>
            builder.AddInt32(1000, rb => rb.AsCubicMetersExpMinus3())); // Liters

        using var cts = new CancellationTokenSource();

        // Act
        var runTask = device.RunAsync(stream, primaryAddress: 0x05, cts.Token);
        await Task.Delay(20, cts.Token);

        var updateException = await Record.ExceptionAsync(async () =>
        {
            await Task.Run(() =>
            {
                for (var i = 0; i < 10; i++)
                {
                    device.UpdateUserData(builder =>
                        builder.AddInt32(2000 + i, rb => rb.AsCubicMetersExpMinus3())); // Liters
                    Thread.Sleep(5);
                }
            }, cts.Token);
        });

        await runTask;

        // Assert
        updateException.ShouldBeNull(); // Updates should not throw
    }

    [Fact]
    public async Task RunAsync_WithCancellationToken_ShouldThrowOperationCanceledException()
    {
        // Arrange
        await using var stream = new BlockingStream();
        var device = TestMbusDevice;
        device.UpdateUserData(_ => { });

        using var cts = new CancellationTokenSource();

        // Act
        var runTask = device.RunAsync(stream, primaryAddress: 0x05, cts.Token);
        await Task.Delay(10, cts.Token);
        await cts.CancelAsync();

        var exception = await Record.ExceptionAsync(async () => await runTask);

        // Assert
        exception.ShouldBeAssignableTo<OperationCanceledException>();
    }

    private class BlockingStream : Stream
    {
        public override async ValueTask<int> ReadAsync(Memory<byte> buffer, CancellationToken cancellationToken = default)
        {
            await Task.Delay(-1, cancellationToken);
            return 0;
        }

        public override void Flush() { }
        public override int Read(byte[] buffer, int offset, int count) => 0;
        public override long Seek(long offset, SeekOrigin origin) => 0;
        public override void SetLength(long value) { }
        public override void Write(byte[] buffer, int offset, int count) { }
        public override bool CanRead => true;
        public override bool CanSeek => false;
        public override bool CanWrite => true;
        public override long Length => 0;
        public override long Position { get; set; }
    }

    [Fact]
    public void RunAsync_WithDifferentPrimaryAddresses_ShouldStartWithoutError()
    {
        // Arrange
        var addresses = new byte[] { 0x01, 0x05, 0x42, 0xFE };
        var exceptions = new List<Exception?>();

        // Act
        foreach (byte addr in addresses)
        {
            var device = TestMbusDevice;
            device.UpdateUserData(_ => { });
            using var stream = new MemoryStream();
            using var cts = new CancellationTokenSource();
            cts.Cancel(); // Cancel immediately

            var exception = Record.Exception(() =>
            {
                var _ = device.RunAsync(stream, primaryAddress: addr, cts.Token);
            });

            exceptions.Add(exception);
        }

        // Assert
        exceptions.ShouldAllBe(e => e == null);
    }
}
