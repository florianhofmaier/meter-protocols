using System.Reflection;
using System.Threading.Channels;
using Metering.Mbus;

// This application references only Metering.Mbus.Devices, never source projects or F# types.
// EN 13757-2, 5.7.7; EN 13757-3, 6.6/7.5: repeat a block with the same FCB,
// advance with a changed FCB and keep one data set until the readout completes.
foreach (var apiType in typeof(MbusDevice).Assembly.GetExportedTypes()
    .Where(type => type.Namespace == "Metering.Mbus"))
{
    foreach (var method in apiType.GetMethods(BindingFlags.Public | BindingFlags.Instance
        | BindingFlags.Static | BindingFlags.DeclaredOnly))
    {
        CheckContractType(method.ReturnType);
        foreach (var parameter in method.GetParameters())
            CheckContractType(parameter.ParameterType);
    }
    foreach (var constructor in apiType.GetConstructors())
        foreach (var parameter in constructor.GetParameters())
            CheckContractType(parameter.ParameterType);
}

var device = MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);
var original = new MbusUserDataBuilder();
for (var i = 0; i < 45; i++)
    original.AddInt32(i, r => r.AsCubicMetersExpMinus3());
var expectedRecords = original.Build();
device.UpdateUserData(original);

using var stream = new DuplexStream();
using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(15));
var running = device.RunAsync(stream, primaryAddress: 5, timeout.Token);

var first = await Read(false);
Require(first[^1] == 0x1F, "First block must announce continuation");
Require(first.AsSpan(0, 5).SequenceEqual(Convert.FromHexString("0DFDFD00E3")), "Missing block identification");
device.UpdateUserData(data => data.AddInt32(999, r => r.AsCubicMetersExpMinus3()));
var repeated = await Read(false);
Require(first.AsSpan().SequenceEqual(repeated), "Repeated FCB changed the block");
var last = await Read(true);
Require(last[^1] != 0x1F, "Last block must finish the readout");
var reconstructed = first[8..^1].Concat(last[8..]).ToArray();
Require(expectedRecords.AsSpan().SequenceEqual(reconstructed), "Update changed an active readout");
var updated = await Read(false);
Require(updated.AsSpan().SequenceEqual(Convert.FromHexString("0413E7030000")), "New readout missed the update");

var custom = new FixedBuilder(Convert.FromHexString("0413D2040000"));
device.UpdateUserData(custom);
try
{
    device.UpdateUserData(new FixedBuilder([4]));
    throw new InvalidOperationException("Invalid builder was accepted");
}
catch (ArgumentException) { }
var afterFailure = await Read(true);
Require(afterFailure.AsSpan().SequenceEqual(custom.Bytes), "Failed update replaced valid data");
stream.Complete();
await running;
Require(!stream.WasDisposed, "Device took ownership of caller stream");

using var blockedStream = new DuplexStream();
using var cancellation = new CancellationTokenSource();
var blocked = device.RunAsync(blockedStream, 5, cancellation.Token);
await cancellation.CancelAsync();
try
{
    await blocked;
    throw new InvalidOperationException("Cancellation did not propagate");
}
catch (OperationCanceledException) { }
Console.WriteLine("Package smoke passed: transitive dependencies, multi-telegram readout, FCB, updates, custom builder, cancellation.");

async Task<byte[]> Read(bool fcb)
{
    var control = fcb ? (byte)0x7B : (byte)0x5B;
    stream.Enqueue([0x10, control, 5, unchecked((byte)(control + 5)), 0x16]);
    var frame = await stream.NextWrite(timeout.Token);
    Require(frame.Length >= 21 && frame[0] == 0x68 && frame[^1] == 0x16, "Invalid response frame");
    Require(frame[1] == frame[2] && frame[1] + 6 == frame.Length, "Invalid response length");
    Require(frame[5] == 5 && frame[6] == 0x72, "Unexpected response address or CI");
    Require(unchecked((byte)frame[4..^2].Sum(b => b)) == frame[^2], "Invalid response checksum");
    return frame[19..^2];
}

static void Require(bool valid, string message)
{
    if (!valid) throw new InvalidOperationException(message);
}

static void CheckContractType(Type type)
{
    if (type.HasElementType)
    {
        CheckContractType(type.GetElementType()!);
        return;
    }
    if (type.IsGenericParameter) return;
    Require(type.Namespace == "Metering.Mbus" || type.Namespace == "System"
        || type.Namespace?.StartsWith("System.", StringComparison.Ordinal) == true,
        $"Implementation type leaked into the C# device API: {type}");
    foreach (var argument in type.GetGenericArguments())
        CheckContractType(argument);
}

sealed class FixedBuilder(byte[] bytes) : IMbusUserDataBuilder
{
    public byte[] Bytes => bytes;
    public byte[] Build() => bytes;
}

sealed class DuplexStream : Stream
{
    private readonly Channel<byte[]> _input = Channel.CreateUnbounded<byte[]>();
    private readonly Channel<byte[]> _output = Channel.CreateUnbounded<byte[]>();
    private byte[]? _pending;
    private int _offset;

    public bool WasDisposed { get; private set; }
    public void Enqueue(byte[] bytes) => _input.Writer.TryWrite(bytes);
    public void Complete() => _input.Writer.TryComplete();
    public ValueTask<byte[]> NextWrite(CancellationToken ct) => _output.Reader.ReadAsync(ct);

    public override async ValueTask<int> ReadAsync(Memory<byte> buffer, CancellationToken ct = default)
    {
        if (_pending is null)
        {
            if (!await _input.Reader.WaitToReadAsync(ct)) return 0;
            _pending = await _input.Reader.ReadAsync(ct);
            _offset = 0;
        }
        var count = Math.Min(buffer.Length, _pending.Length - _offset);
        _pending.AsMemory(_offset, count).CopyTo(buffer);
        _offset += count;
        if (_offset == _pending.Length) _pending = null;
        return count;
    }

    public override ValueTask WriteAsync(ReadOnlyMemory<byte> buffer, CancellationToken ct = default)
    {
        ct.ThrowIfCancellationRequested();
        _output.Writer.TryWrite(buffer.ToArray());
        return ValueTask.CompletedTask;
    }

    protected override void Dispose(bool disposing)
    {
        WasDisposed = true;
        base.Dispose(disposing);
    }

    public override bool CanRead => true;
    public override bool CanWrite => true;
    public override bool CanSeek => false;
    public override long Length => throw new NotSupportedException();
    public override long Position { get => throw new NotSupportedException(); set => throw new NotSupportedException(); }
    public override void Flush() { }
    public override int Read(byte[] buffer, int offset, int count) => throw new NotSupportedException();
    public override void Write(byte[] buffer, int offset, int count) => throw new NotSupportedException();
    public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();
    public override void SetLength(long value) => throw new NotSupportedException();
}
