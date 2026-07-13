namespace Metering.Mbus;

public interface IMbusUserDataBuilder
{
    IMbusUserDataBuilder AddInt32(int value, Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc);
}

public interface IMbusRecordWithValueBuilder
{
    IMbusRecordWithUnitBuilder AsCubicMetersExpMinus6();
    IMbusRecordWithUnitBuilder AsCubicMetersExpMinus5();
    IMbusRecordWithUnitBuilder AsCubicMetersExpMinus4();
    IMbusRecordWithUnitBuilder AsCubicMetersExpMinus3();
    IMbusRecordWithUnitBuilder AsCubicMetersExpMinus2();
    IMbusRecordWithUnitBuilder AsCubicMetersExpMinus1();
    IMbusRecordWithUnitBuilder AsCubicMetersExp0();
    IMbusRecordWithUnitBuilder AsCubicMetersExp1();
    IMbusRecordWithUnitBuilder AsCubicMetersExp2();
    IMbusRecordWithUnitBuilder AsCubicMetersExp3();
}

public interface IMbusRecordWithUnitBuilder
{
    IMbusRecordWithUnitBuilder WithFunctionField(MbusFunctionField function);
    IMbusRecordWithUnitBuilder WithStorageNumber(int storageNumber);
    IMbusRecordWithUnitBuilder WithSubUnit(int subUnit);
    IMbusRecordWithUnitBuilder WithTariff(int tariff);
}