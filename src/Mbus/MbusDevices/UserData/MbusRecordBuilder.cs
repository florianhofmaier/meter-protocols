namespace Metering.Mbus.UserData;

public class RecordBuilderWithValue(MbusValue value) : IMbusRecordWithValueBuilder
{
    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus6() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExpMinus6(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus5() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExpMinus5(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus4() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExpMinus4(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus3() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExpMinus3(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus2() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExpMinus2(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExpMinus1() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExpMinus1(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExp0() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExp0(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExp1() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExp1(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExp2() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExp2(value));

    public IMbusRecordWithUnitBuilder AsCubicMetersExp3() =>
        new RecordBuilderWithUnit(RecordsBuilders.VolumeRecords.asCubicMetersExp3(value));

    public RecordBuilderWithUnit AsFabricationNumber() =>
        new (RecordsBuilders.FabricationNumbers.asFabNum(value));
}

public class RecordBuilderWithUnit(RspDataRecord record) : IMbusRecordWithUnitBuilder
{
    public RspDataRecord RspRecord => record;

    public IMbusRecordWithUnitBuilder WithFunctionField(MbusFunctionField function) =>
        new RecordBuilderWithUnit(RecordsBuilders.withFunction<RspDataRecord>(function, record).ResultValue);

    public IMbusRecordWithUnitBuilder WithStorageNumber(int storageNumber) =>
        RecordsBuilders.withStNum(storageNumber, record)
            .MapOrThrow(
                r => new RecordBuilderWithUnit(r),
                e => new MbusError(e));

    public IMbusRecordWithUnitBuilder WithSubUnit(int subUnit) =>
        RecordsBuilders.withSubUnit(subUnit, record)
            .MapOrThrow(
                r => new RecordBuilderWithUnit(r),
                e => new MbusError(e));

    public IMbusRecordWithUnitBuilder WithTariff(int tariff) =>
        RecordsBuilders.withTariff(tariff, record)
            .MapOrThrow(
                r => new RecordBuilderWithUnit(r),
                e => new MbusError(e));
}