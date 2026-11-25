namespace Metering.Mbus;

public interface IMbusUserDataBuilder
{
    /// <summary>
    /// Builds a complete response APL data set without CI, transport or link-layer framing.
    /// MbusDevice calls this once per explicit UpdateUserData call and copies the returned bytes.
    /// The output must be non-null and stable until the update returns. An empty array clears the data.
    /// </summary>
    byte[] Build();
}

public interface IMbusRecordWithValueBuilder
{
    /// <summary>
    /// Selects an exact value type, unit and multiplier from the codec's standard VIF tables.
    /// The supplied value is the encoded value; this does not rescale it.
    /// </summary>
    /// <remarks>
    /// Unsupported combinations are rejected when the record is added. VIFs for compound data
    /// types, approval classes and protocol containers require a typed API and are not supported here.
    /// Other VIF-specific data interpretation remains the caller's responsibility.
    /// </remarks>
    IMbusRecordWithUnitBuilder As(MbusValueType valueType, MbusUnit unit, decimal scaler = 1m);

    /// <summary>Selects a plain-text VIF with at most 255 ASCII characters and no extensions.</summary>
    IMbusRecordWithUnitBuilder AsTextUnit(string unit);

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
    IMbusRecordWithUnitBuilder AsFabricationNumber();
}

public interface IMbusRecordWithUnitBuilder
{
    /// <summary>Appends a record-local error VIFE to a standard VIF.</summary>
    /// <remarks>Plain-text VIFs with error extensions are not represented by the current model.</remarks>
    IMbusRecordWithUnitBuilder WithRecordError(MbusRecordError error);

    IMbusRecordWithUnitBuilder WithFunctionField(MbusFunctionField function);
    IMbusRecordWithUnitBuilder WithStorageNumber(int storageNumber);
    IMbusRecordWithUnitBuilder WithSubUnit(int subUnit);
    IMbusRecordWithUnitBuilder WithTariff(int tariff);
}
