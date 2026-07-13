using Metering.Mbus.Devices;
using Metering.Mbus.UserData;
using Microsoft.FSharp.Collections;
using Microsoft.FSharp.Core;

namespace Metering.Mbus;

public class MbusDevice
{
    private readonly Lock _lock = new();
    private readonly MbusAddress _secondaryAddress;
    private readonly List<RspDataRecord> _userData = [];

    private MbusDevice(MbusAddress secondaryAddress) => _secondaryAddress = secondaryAddress;

    public static MbusDevice Create(int idNumber, string mfr, int version, MbusDeviceType deviceType) =>
        MbusAddressModule.create(idNumber, mfr, (byte)version, deviceType) switch
        {
            { IsError: true } err => throw new ArgumentException(err.ErrorValue),
            { ResultValue: var address } => new MbusDevice(address)
        };

    public void UpdateUserData(Action<IMbusUserDataBuilder> updateAction)
    {
        lock (_lock)
        {
            _userData.Clear();
            var builder = new MbusUserDataBuilder(_userData);
            updateAction(builder);
        }
    }

    public Task RunAsync(
        Stream stream,
        byte primaryAddress,
        CancellationToken ct)
    {
        var initialState = DeviceLogic.initialState(primaryAddress, _secondaryAddress);

        return DeviceRuntime.runAsync(stream, GetDataFunc, initialState, ct);
    }

    private FSharpFunc<Unit, FSharpList<RspDataRecord>> GetDataFunc =>
        FuncConvert.FromFunc(() =>
        {
            lock (_lock)
            {
                return ListModule.OfSeq(_userData);
            }
        });
}