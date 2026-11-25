using Metering.Mbus.Codec;
using Metering.Mbus.Devices;
using Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus;
using Metering.Mbus.Protocol.Frames.DeviceIdentification;
using Metering.Mbus.Protocol.Frames.TransportLayer.Security;
using Metering.Mbus.Protocol.Messages;
using Metering.Mbus.Protocol.Records;
using Metering.Mbus.Protocol.Frames.ApplicationLayer;
using Metering.Mbus.UserData;
using Microsoft.FSharp.Core;
using static Metering.Common.Decoding.Decoders.Core;
using static Metering.Common.Decoding.Validators.Core;

namespace Metering.Mbus;

public class MbusDevice
{
    private readonly Lock _lock = new();
    private readonly MeterAddress _secondaryAddress;
    private RspUdData _userData = RspUdDataModule.create(
        Array.Empty<DataRecordRsp>(), FSharpOption<MfrSpecificData>.None, false);

    private MbusDevice(MeterAddress secondaryAddress) => _secondaryAddress = secondaryAddress;

    public static MbusDevice Create(int idNumber, string mfr, int version, MbusDeviceType deviceType)
    {
        if (!Enum.IsDefined(deviceType))
            throw new ArgumentOutOfRangeException(nameof(deviceType));

        var address = MeterAddressModule.create(idNumber, mfr, version, (int)deviceType)
            .ValueOrThrow();
        return new MbusDevice(address);
    }

    /// <summary>
    /// Builds and atomically replaces the complete user-data set using the fluent builder.
    /// Failed updates leave the current data and any active readout intact.
    /// </summary>
    public void UpdateUserData(Action<MbusUserDataBuilder> updateAction)
    {
        ArgumentNullException.ThrowIfNull(updateAction);
        var builder = new MbusUserDataBuilder();
        updateAction(builder);
        UpdateUserData(builder);
    }

    /// <summary>
    /// Calls a custom builder once and copies, validates and atomically replaces its complete APL data.
    /// The builder is not retained or called during readout. Exceptions are reported by this call;
    /// previously published data and active readouts remain available when an update fails.
    /// </summary>
    /// <exception cref="ArgumentNullException">The builder is null.</exception>
    /// <exception cref="ArgumentException">The output is null, invalid, incomplete or cannot be sent.</exception>
    public void UpdateUserData(IMbusUserDataBuilder builder)
    {
        ArgumentNullException.ThrowIfNull(builder);
        var bytes = builder.Build()
            ?? throw new ArgumentException("The builder must return a byte array", nameof(builder));
        var decoded = Codec.UserData.decode(new ReadOnlyMemory<byte>(bytes.ToArray()));
        var snapshot = decoded switch
        {
            DecodeResult<RspUdData>.Decoded success => success.Item1,
            DecodeResult<RspUdData>.DecodeFailed failure => throw new ArgumentException(
                DescribeFailure(failure.Item1), nameof(builder)),
            _ => throw new InvalidOperationException("Unknown user-data decoding result")
        };

        if (RspUdDataModule.moreFollows(snapshot))
            throw new ArgumentException(
                "Supply the complete data set without a more-follows marker", nameof(builder));

        // Both supported response forms must be sendable before publishing the data.
        foreach (var selectedApplication in new[] { false, true })
        {
            var readout = Readout.prepare(selectedApplication, snapshot);
            if (readout.IsError)
                throw new ArgumentException(readout.ErrorValue, nameof(builder));
        }

        lock (_lock)
        {
            _userData = snapshot;
        }
    }

    public Task RunAsync(Stream stream, byte primaryAddress, CancellationToken ct)
    {
        ArgumentNullException.ThrowIfNull(stream);
        var address = primaryAddress == 0
            ? PrimaryAddress.Unconfigured
            : PrimAdrModule.tryCreate(primaryAddress) is { Value: var configured }
                ? PrimaryAddress.NewPrimAdr(configured)
                : throw new ArgumentOutOfRangeException(
                    nameof(primaryAddress), "Device primary address must be between 0 and 250");

        var initialState = DeviceLogic.initialState(address, _secondaryAddress);
        var resolver = new NoSecurityContext();
        var encode = FuncConvert.FromFunc((EncodingContext context, Message message) =>
            Encoder.create(EncodingOptions.Plain, resolver, context, message));
        return DeviceRuntime.runAsync(stream, encode, GetDataFunc, initialState, ct);
    }

    private static string DescribeFailure(DecodeFailure failure) =>
        failure switch
        {
            DecodeFailure.ParseFailed parse =>
                $"Invalid user data at byte {parse.Item.Pos}: {parse.Item.Msg}",
            DecodeFailure.ValidationFailed validation => string.Join(
                "; ", FailuresModule.toList(validation.Item).Select(issue => issue.Message)),
            DecodeFailure.EncryptionFailed protection => protection.Item.Message,
            _ => "Invalid user data"
        };

    private FSharpFunc<Unit, RspUdData> GetDataFunc =>
        FuncConvert.FromFunc(() =>
        {
            lock (_lock)
            {
                return _userData;
            }
        });

    private sealed class NoSecurityContext : IExternalSecurityContextResolver
    {
        public FSharpOption<Mode5ExternalContext> ResolveMode5Context(MeterAddress identity) =>
            FSharpOption<Mode5ExternalContext>.None;
    }
}
