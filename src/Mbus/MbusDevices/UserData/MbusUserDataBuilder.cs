using Metering.Mbus.Protocol.Records;
using Metering.Mbus.UserData;

namespace Metering.Mbus;

/// <summary>
/// Builds response application data using raw wire values. Unit scalers describe the values;
/// they do not convert them. Unsupported encodings are rejected before changing the data set.
/// </summary>
public sealed class MbusUserDataBuilder : IMbusUserDataBuilder
{
    private readonly List<DataRecordRsp> _records = [];

    // EN 13757-3:2025, 6.3.3, Tables 4/5: DIF representations and LVAR encodings.
    public MbusUserDataBuilder AddInt8(
        sbyte value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewInt8(value), recordBuilderFunc);

    public MbusUserDataBuilder AddInt16(
        short value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewInt16(value), recordBuilderFunc);

    public MbusUserDataBuilder AddInt24(
        int value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewInt24(value), recordBuilderFunc);

    public MbusUserDataBuilder AddInt32(
        int value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewInt32(value), recordBuilderFunc);

    public MbusUserDataBuilder AddInt48(
        long value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewInt48(value), recordBuilderFunc);

    public MbusUserDataBuilder AddInt64(
        long value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewInt64(value), recordBuilderFunc);

    public MbusUserDataBuilder AddReal32(
        float value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewReal32(value), recordBuilderFunc);

    public MbusUserDataBuilder AddBcd2Digit(
        byte value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewBcd2Digit(value), recordBuilderFunc);

    public MbusUserDataBuilder AddBcd4Digit(
        ushort value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewBcd4Digit(value), recordBuilderFunc);

    public MbusUserDataBuilder AddBcd6Digit(
        uint value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewBcd6Digit(value), recordBuilderFunc);

    public MbusUserDataBuilder AddBcd8Digit(
        uint value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewBcd8Digit(value), recordBuilderFunc);

    public MbusUserDataBuilder AddBcd12Digit(
        ulong value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewBcd12Digit(value), recordBuilderFunc);

    public MbusUserDataBuilder AddPositiveBcd(
        ulong value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewPosBcd(value), recordBuilderFunc);

    public MbusUserDataBuilder AddNegativeBcd(
        long value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewNegBcd(value), recordBuilderFunc);

    /// <summary>Adds up to 191 ISO-8859-1 characters using the variable-length text encoding.</summary>
    public MbusUserDataBuilder AddText(
        string value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc)
    {
        ArgumentNullException.ThrowIfNull(value);
        return Add(Value.NewText(value), recordBuilderFunc);
    }

    /// <summary>
    /// Adds binary bytes in wire order using LVAR. Supported lengths are 0–15, 16, 20, 24,
    /// 28, 32, 48 and 64 bytes. The input is copied before the record is added.
    /// </summary>
    public MbusUserDataBuilder AddBinary(
        ReadOnlyMemory<byte> value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NewBinary(new ReadOnlyMemory<byte>(value.ToArray())), recordBuilderFunc);

    /// <summary>Adds a record with DIF data field zero and no value bytes.</summary>
    public MbusUserDataBuilder AddNoData(
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc) =>
        Add(Value.NoData, recordBuilderFunc);

    private MbusUserDataBuilder Add(
        Value value,
        Func<IMbusRecordWithValueBuilder, IMbusRecordWithUnitBuilder> recordBuilderFunc)
    {
        ArgumentNullException.ThrowIfNull(recordBuilderFunc);
        var supplied = new RecordBuilderWithValue(value);
        var record = recordBuilderFunc(supplied);
        if (record is not RecordBuilderWithUnit built || !ReferenceEquals(built.Owner, supplied))
            throw new ArgumentException(
                "Use the record builder supplied to the callback", nameof(recordBuilderFunc));

        var result = Codec.UserData.encode(new[] { built.Record });
        if (result.IsError)
            throw new ArgumentException($"Cannot encode record: {result.ErrorValue.Msg}");

        _records.Add(built.Record);
        return this;
    }

    /// <summary>Encodes all records into an independent byte array without a telegram-size limit.</summary>
    public byte[] Build()
    {
        var result = Codec.UserData.encode(_records);
        if (result.IsError)
            throw new InvalidOperationException($"Cannot encode user data: {result.ErrorValue.Msg}");

        return result.ResultValue;
    }
}
