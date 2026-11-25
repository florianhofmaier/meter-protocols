using Metering.Mbus.Protocol.Records;
using Metering.Mbus.Protocol.Records.DataInfoBlocks;
using Metering.Mbus.Protocol.Records.ValueInfoBlocks;
using Microsoft.FSharp.Collections;

namespace Metering.Mbus.UserData;

internal sealed class RecordBuilderWithValue(Value value) : IMbusRecordWithValueBuilder
{
    public IMbusRecordWithUnitBuilder As(
        MbusValueType valueType, MbusUnit unit, decimal scaler = 1m)
    {
        // EN 13757-3:2025, Tables 10/12, A.5/A.6/A.8-A.12 and 6.5.4:
        // compound encodings, approval classes and protocol containers need distinct value models.
        if (valueType == MbusValueType.Date
            || valueType == MbusValueType.DateAndTime
            || valueType == MbusValueType.StartOfTariff
            || valueType == MbusValueType.DateAndTimeOfBatteryChange
            || valueType == MbusValueType.DaylightSavings
            || valueType == MbusValueType.ListeningWindowManagement
            || valueType == MbusValueType.TypeOrClassOfApproval
            || valueType == MbusValueType.DataContainerForWMbusProtocol
            || valueType == MbusValueType.DataContainerForManufacturerSpecificProtocol
            || valueType == MbusValueType.DataContainerForMbusUpperLayers)
            throw new NotSupportedException(
                "VIFs for compound data types, approval classes and protocol containers require a typed API");

        return Build(new Vif(valueType, unit, scaler));
    }

    public IMbusRecordWithUnitBuilder AsTextUnit(string unit)
    {
        // EN 13757-3:2025, 6.4.1/Table 11 and C.2: ASCII VIF with a one-byte length.
        ArgumentNullException.ThrowIfNull(unit);
        if (unit.Length > 255 || unit.Any(c => c > 127))
            throw new ArgumentException(
                "A plain-text VIF requires at most 255 ASCII characters", nameof(unit));

        var dib = new Dib(
            FunctionField.Inst, StorageNumberModule.zero, TariffModule.zero, SubUnitModule.zero);
        return new RecordBuilderWithUnit(new DataRecordRsp(dib, VibRsp.NewText(unit), value))
        {
            Owner = this
        };
    }

    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus6() => Volume(0.000001m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus5() => Volume(0.00001m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus4() => Volume(0.0001m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus3() => Volume(0.001m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus2() => Volume(0.01m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus1() => Volume(0.1m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExp0() => Volume(1m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExp1() => Volume(10m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExp2() => Volume(100m);
    public IMbusRecordWithUnitBuilder AsCubicMetersExp3() => Volume(1000m);
    public IMbusRecordWithUnitBuilder AsFabricationNumber() =>
        Build(new Vif(MbusValueType.FabricationNumber, MbusUnit.NoUnit, 1m));

    private RecordBuilderWithUnit Volume(decimal scaler) =>
        Build(new Vif(MbusValueType.Volume, MbusUnit.CubicMeters, scaler));

    private RecordBuilderWithUnit Build(Vif vif)
    {
        var dib = new Dib(
            FunctionField.Inst, StorageNumberModule.zero, TariffModule.zero, SubUnitModule.zero);
        var vib = VibRsp.NewNormal(new NormalRspVib(
            vif, FSharpList<VifExtension>.Empty, FSharpList<RecordError>.Empty));
        return new RecordBuilderWithUnit(new DataRecordRsp(dib, vib, value)) { Owner = this };
    }
}

internal sealed class RecordBuilderWithUnit(DataRecordRsp record) : IMbusRecordWithUnitBuilder
{
    internal DataRecordRsp Record => record;
    internal object? Owner { get; init; }

    public IMbusRecordWithUnitBuilder WithRecordError(MbusRecordError error)
    {
        // EN 13757-3:2025, 6.4.8/Table 19: errors belonging to this record are VIFEs.
        if (!Enum.IsDefined(error))
            throw new ArgumentOutOfRangeException(nameof(error));
        if (record.Vib is not VibRsp.Normal normal)
            throw new NotSupportedException(
                "The current plain-text VIF model does not support record-error extensions");

        var code = RecordErrorModule.tryMap(new ReadOnlyMemory<byte>([(byte)error]), 0).Item1;
        if (code is null)
            throw new ArgumentOutOfRangeException(nameof(error));

        var vib = VibRsp.NewNormal(new NormalRspVib(
            normal.Item.Def,
            normal.Item.Ext,
            ListModule.OfSeq(normal.Item.Codes.Append(code.Value))));
        return new RecordBuilderWithUnit(new DataRecordRsp(record.Dib, vib, record.Value))
        {
            Owner = Owner
        };
    }

    public IMbusRecordWithUnitBuilder WithFunctionField(MbusFunctionField function)
    {
        var field = function switch
        {
            MbusFunctionField.InstValue => FunctionField.Inst,
            MbusFunctionField.MaxValue => FunctionField.Max,
            MbusFunctionField.MinValue => FunctionField.Min,
            MbusFunctionField.ValueInErrorState => FunctionField.Err,
            _ => throw new ArgumentOutOfRangeException(nameof(function))
        };
        var dib = new Dib(field, record.Dib.StNum, record.Dib.Tariff, record.Dib.SubUnit);
        return new RecordBuilderWithUnit(new DataRecordRsp(dib, record.Vib, record.Value))
        {
            Owner = Owner
        };
    }

    public IMbusRecordWithUnitBuilder WithStorageNumber(int storageNumber)
    {
        var value = StorageNumberModule.create(storageNumber).ValueOrThrow(nameof(storageNumber));
        var dib = new Dib(record.Dib.Fn, value, record.Dib.Tariff, record.Dib.SubUnit);
        return new RecordBuilderWithUnit(new DataRecordRsp(dib, record.Vib, record.Value))
        {
            Owner = Owner
        };
    }

    public IMbusRecordWithUnitBuilder WithSubUnit(int subUnit)
    {
        var value = SubUnitModule.create(subUnit).ValueOrThrow(nameof(subUnit));
        var dib = new Dib(record.Dib.Fn, record.Dib.StNum, record.Dib.Tariff, value);
        return new RecordBuilderWithUnit(new DataRecordRsp(dib, record.Vib, record.Value))
        {
            Owner = Owner
        };
    }

    public IMbusRecordWithUnitBuilder WithTariff(int tariff)
    {
        var value = TariffModule.create(tariff).ValueOrThrow(nameof(tariff));
        var dib = new Dib(record.Dib.Fn, record.Dib.StNum, value, record.Dib.SubUnit);
        return new RecordBuilderWithUnit(new DataRecordRsp(dib, record.Vib, record.Value))
        {
            Owner = Owner
        };
    }
}
