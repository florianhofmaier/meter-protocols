namespace Metering.Mbus.UserData;

public class MbusUserDataBuilder(List<RspDataRecord> userData) : IMbusUserDataBuilder
{
    public IMbusUserDataBuilder AddInt32(
        int value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        RecordsBuilders.Values.withInt32<string>(value)
            .MapOrThrow(
                v =>
                {
                    var recordBuilderWithUnit = (RecordBuilderWithUnit) recordBuilderFunc(new RecordBuilderWithValue(v));
                    userData.Add(recordBuilderWithUnit.RspRecord);
                    return this;
                },
                e => throw new MbusError(e));
}