using Shouldly;

namespace Metering.Mbus.MbusDevices.Tests;

public class ExtendedBuilderTests
{
    [Fact]
    public void Build_EncodesPrimitiveRepresentationsWithoutChangingTheRawValue()
    {
        // EN 13757-3:2025, 6.3.3/Tables 4–5 and Annex A: DIF, LVAR, little-endian data.
        var builder = new MbusUserDataBuilder()
            .AddInt8(-2, Volume)
            .AddInt16(-1234, Volume)
            .AddInt24(-1234, Volume)
            .AddInt32(-1234, Volume)
            .AddInt48(0x010203040506, Volume)
            .AddInt64(0x0102030405060708, Volume)
            .AddReal32(1.5f, Volume)
            .AddBcd2Digit(12, Volume)
            .AddBcd4Digit(1234, Volume)
            .AddBcd6Digit(123456, Volume)
            .AddBcd8Digit(12345678, Volume)
            .AddBcd12Digit(123456789012, Volume)
            .AddText("ä", Volume)
            .AddPositiveBcd(123456, Volume)
            .AddNegativeBcd(-123456, Volume)
            .AddNoData(record => Volume(record)
                .WithFunctionField(MbusFunctionField.ValueInErrorState));

        builder.Build().ShouldBe(Convert.FromHexString(string.Concat(
            "0116FE", "02162EFB", "03162EFBFF", "04162EFBFFFF",
            "0616060504030201", "07160807060504030201", "05160000C03F",
            "091612", "0A163412", "0B16563412", "0C1678563412",
            "0E16129078563412", "0D1601E4", "0D16C3563412", "0D16D3563412", "3016")));
    }

    [Fact]
    public void Build_EncodesValidWidthBoundariesWithoutTruncation()
    {
        // Annex A/Table A.2: the lowest signed bit pattern denotes invalid data, so use min + 1.
        var builder = new MbusUserDataBuilder()
            .AddInt24(-8388607, Volume)
            .AddInt24(8388607, Volume)
            .AddInt48(-140737488355327, Volume)
            .AddInt48(140737488355327, Volume)
            .AddBcd2Digit(99, Volume)
            .AddBcd4Digit(9999, Volume)
            .AddBcd6Digit(999999, Volume)
            .AddBcd8Digit(99999999, Volume)
            .AddBcd12Digit(999999999999, Volume)
            .AddPositiveBcd(999999999999999999, Volume)
            .AddNegativeBcd(-999999999999999999, Volume);

        builder.Build().ShouldBe(Convert.FromHexString(string.Concat(
            "0316010080", "0316FFFF7F", "0616010000000080", "0616FFFFFFFFFF7F",
            "091699", "0A169999", "0B16999999", "0C1699999999", "0E16999999999999",
            "0D16C9999999999999999999", "0D16D9999999999999999999")));
    }

    [Fact]
    public void Add_RejectsUnencodableValuesWithoutChangingEarlierRecords()
    {
        var builder = new MbusUserDataBuilder().AddInt32(1, Volume);
        var previous = builder.Build();
        Action[] invalidAdds =
        [
            () => builder.AddInt24(-8388609, Volume),
            () => builder.AddInt24(8388608, Volume),
            () => builder.AddInt48(-140737488355329, Volume),
            () => builder.AddInt48(140737488355328, Volume),
            () => builder.AddBcd2Digit(100, Volume),
            () => builder.AddBcd4Digit(10000, Volume),
            () => builder.AddBcd6Digit(1000000, Volume),
            () => builder.AddBcd8Digit(100000000, Volume),
            () => builder.AddBcd12Digit(1000000000000, Volume),
            () => builder.AddPositiveBcd(1000000000000000000, Volume),
            () => builder.AddNegativeBcd(-1000000000000000000, Volume),
            () => builder.AddNegativeBcd(1, Volume),
            () => builder.AddText(new string('A', 192), Volume),
            () => builder.AddText("€", Volume)
        ];

        foreach (var add in invalidAdds)
        {
            Should.Throw<ArgumentException>(add);
            builder.Build().ShouldBe(previous);
        }
    }

    [Theory]
    [InlineData(0, 0xE0)]
    [InlineData(1, 0xE1)]
    [InlineData(15, 0xEF)]
    [InlineData(16, 0xF0)]
    [InlineData(20, 0xF1)]
    [InlineData(24, 0xF2)]
    [InlineData(28, 0xF3)]
    [InlineData(32, 0xF4)]
    [InlineData(48, 0xF5)]
    [InlineData(64, 0xF6)]
    public void AddBinary_UsesTheDeclaredLvarLengthAndCopiesInput(int length, int lvar)
    {
        // EN 13757-3:2025, Table 5: the binary length map is discontinuous after 15 bytes.
        var input = Enumerable.Range(0, length).Select(i => (byte)i).ToArray();
        var expected = new byte[] { 0x0D, 0x16, (byte)lvar }.Concat(input).ToArray();
        var builder = new MbusUserDataBuilder().AddBinary(input, Volume);

        Array.Fill(input, (byte)0xFF);

        builder.Build().ShouldBe(expected);
        var result = builder.Build();
        Array.Fill(result, (byte)0);
        builder.Build().ShouldBe(expected);
    }

    [Theory]
    [InlineData(17)]
    [InlineData(19)]
    [InlineData(33)]
    [InlineData(47)]
    [InlineData(49)]
    [InlineData(65)]
    public void AddBinary_RejectsLengthsWithoutAnLvarCode(int length)
    {
        var builder = new MbusUserDataBuilder().AddInt32(1, Volume);
        var previous = builder.Build();

        Should.Throw<ArgumentException>(() => builder.AddBinary(new byte[length], Volume));

        builder.Build().ShouldBe(previous);
    }

    [Fact]
    public void AddText_AcceptsTheMaximumLatin1Length()
    {
        var builder = new MbusUserDataBuilder().AddText(new string('ä', 191), Volume);

        builder.Build().ShouldBe(new byte[] { 0x0D, 0x16, 0xBF }
            .Concat(Enumerable.Repeat((byte)0xE4, 191)).ToArray());
    }

    [Fact]
    public void Build_SelectsPrimaryAndExtendedVifsWithoutScalingTheStoredValue()
    {
        // EN 13757-3:2025, Tables 10/12/15: Wh, W, °C, voltage and extended volume.
        var builder = new MbusUserDataBuilder()
            .AddInt32(1234, record => record.As(MbusValueType.Energy, MbusUnit.WattHours, 1000m))
            .AddInt16(42, record => record.As(MbusValueType.Power, MbusUnit.Watts))
            .AddReal32(1.5f, record => record.As(
                MbusValueType.FlowTemperature, MbusUnit.Celsius, 0.01m))
            .AddInt16(230, record => record.As(MbusValueType.Voltage, MbusUnit.Volts))
            .AddInt16(2, record => record.As(MbusValueType.Volume, MbusUnit.CubicMeters, 100m));

        builder.Build().ShouldBe(Convert.FromHexString(string.Concat(
            "0406D2040000", "022B2A00", "05590000C03F", "02FD49E600", "02FB100200")));
    }

    [Fact]
    public void As_RejectsUnknownQuantityUnitAndScaleCombinations()
    {
        var builder = new MbusUserDataBuilder().AddInt32(1, Volume);
        var previous = builder.Build();

        Should.Throw<ArgumentException>(() => builder.AddInt32(1,
            record => record.As(MbusValueType.Volume, MbusUnit.Watts)));
        Should.Throw<ArgumentException>(() => builder.AddInt32(1,
            record => record.As(MbusValueType.Volume, MbusUnit.CubicMeters, 0.3m)));

        builder.Build().ShouldBe(previous);
    }

    [Fact]
    public void As_RejectsCompoundVifsThatNeedTypedPayloads()
    {
        // EN 13757-3:2025, Tables 10/12 and 6.5.4: compound types require specific DIF/LVAR.
        var builder = new MbusUserDataBuilder().AddInt32(1, Volume);
        var previous = builder.Build();
        MbusValueType[] compoundTypes =
        [
            MbusValueType.Date,
            MbusValueType.DateAndTime,
            MbusValueType.StartOfTariff,
            MbusValueType.DateAndTimeOfBatteryChange,
            MbusValueType.DaylightSavings,
            MbusValueType.ListeningWindowManagement,
            MbusValueType.TypeOrClassOfApproval,
            MbusValueType.DataContainerForWMbusProtocol,
            MbusValueType.DataContainerForManufacturerSpecificProtocol,
            MbusValueType.DataContainerForMbusUpperLayers
        ];

        foreach (var type in compoundTypes)
            Should.Throw<NotSupportedException>(() => builder.AddReal32(1f,
                record => record.As(type, MbusUnit.NoUnit)));

        builder.Build().ShouldBe(previous);
    }

    [Fact]
    public void As_AllowsScalarTimePointSecond()
    {
        // EN 13757-3:2025, Table 12: FD 2B is the unsigned second (0–59).
        new MbusUserDataBuilder()
            .AddBcd2Digit(59, record => record.As(MbusValueType.TimePointSecond, MbusUnit.Seconds))
            .Build().ShouldBe(Convert.FromHexString("09FD2B59"));
    }

    [Fact]
    public void WithRecordError_PreservesErrorOrderAndDibChanges()
    {
        // EN 13757-3:2025, 6.4.8/Table 19 and 6.3.5: VIFEs and storage number bit.
        new MbusUserDataBuilder().AddNoData(record => record.AsFabricationNumber()
                .WithRecordError(MbusRecordError.DataOverflow)
                .WithRecordError(MbusRecordError.NoDataAvailable)
                .WithStorageNumber(1))
            .Build().ShouldBe(Convert.FromHexString("40F89615"));
    }

    [Fact]
    public void WithRecordError_RejectsMoreThanTenVifesWithoutAddingTheRecord()
    {
        // EN 13757-3:2025, 6.4.1: at most ten VIFEs in addition to the VIF.
        var builder = new MbusUserDataBuilder();
        builder.AddNoData(record =>
        {
            var unit = record.AsFabricationNumber();
            for (var i = 0; i < 10; i++)
                unit = unit.WithRecordError(MbusRecordError.DataError);
            return unit;
        });
        var previous = new byte[] { 0x00, 0xF8 }
            .Concat(Enumerable.Repeat((byte)0x98, 9)).Append((byte)0x18).ToArray();
        builder.Build().ShouldBe(previous);

        Should.Throw<ArgumentException>(() => builder.AddNoData(record =>
        {
            var unit = record.AsFabricationNumber();
            for (var i = 0; i < 11; i++)
                unit = unit.WithRecordError(MbusRecordError.DataError);
            return unit;
        }));
        builder.Build().ShouldBe(previous);
    }

    [Fact]
    public void WithRecordError_EncodesMissingDataAsARecordErrorVife()
    {
        // EN 13757-3:2025, 6.4.8/Table 19: no-data DIF and error VIFE 15h.
        var builder = new MbusUserDataBuilder().AddNoData(record =>
            record.AsCubicMetersExpMinus3().WithRecordError(MbusRecordError.NoDataAvailable));

        builder.Build().ShouldBe(Convert.FromHexString("009315"));
    }

    [Fact]
    public void WithRecordError_RejectsUnknownCodesAndUnmodeledTextVifExtensions()
    {
        var builder = new MbusUserDataBuilder().AddInt32(1, Volume);
        var previous = builder.Build();

        Should.Throw<ArgumentOutOfRangeException>(() => builder.AddNoData(record =>
            Volume(record).WithRecordError((MbusRecordError)0x7F)));
        Should.Throw<NotSupportedException>(() => builder.AddNoData(record =>
            record.AsTextUnit("iGal").WithRecordError(MbusRecordError.NoDataAvailable)));

        builder.Build().ShouldBe(previous);
    }

    [Fact]
    public void AsTextUnit_EncodesRightmostCharacterFirst()
    {
        // EN 13757-3:2025, C.2/Table C.3: custom-unit text is transmitted in reverse order.
        var builder = new MbusUserDataBuilder()
            .AddBcd8Digit(75420826, record => record.AsTextUnit("iGal"));

        builder.Build().ShouldBe(Convert.FromHexString("0C7C046C61476926084275"));
    }

    [Fact]
    public void AsTextUnit_AcceptsOneByteLengthAndRejectsNonAscii()
    {
        var builder = new MbusUserDataBuilder()
            .AddInt8(1, record => record.AsTextUnit(new string('A', 255)));
        var expected = new byte[] { 0x01, 0x7C, 0xFF }
            .Concat(Enumerable.Repeat((byte)'A', 255)).Append((byte)1).ToArray();

        builder.Build().ShouldBe(expected);
        Should.Throw<ArgumentException>(() => builder.AddInt8(1,
            record => record.AsTextUnit(new string('A', 256))));
        Should.Throw<ArgumentException>(() => builder.AddInt8(1,
            record => record.AsTextUnit("m³")));
        Should.Throw<ArgumentNullException>(() => builder.AddInt8(1,
            record => record.AsTextUnit(null!)));
        builder.Build().ShouldBe(expected);
    }

    [Fact]
    public void Add_PropagatesCallbackErrorsAndRejectsNullValues()
    {
        var builder = new MbusUserDataBuilder().AddInt32(1, Volume);
        var previous = builder.Build();
        var failure = new InvalidOperationException("callback failed");

        Should.Throw<InvalidOperationException>(() => builder.AddInt16(1, _ => throw failure))
            .ShouldBeSameAs(failure);
        Should.Throw<ArgumentNullException>(() => builder.AddInt16(1, null!));
        Should.Throw<ArgumentException>(() => builder.AddNoData(_ => null!));
        Should.Throw<ArgumentNullException>(() => builder.AddText(null!, Volume));

        builder.Build().ShouldBe(previous);
    }

    [Fact]
    public void Add_RejectsRecordsCapturedFromAnotherCallback()
    {
        var builder = new MbusUserDataBuilder();
        IMbusRecordWithUnitBuilder? captured = null;
        builder.AddInt32(1, record => captured = Volume(record));
        var previous = builder.Build();

        Should.Throw<ArgumentException>(() => builder.AddInt32(2, _ => captured!));
        Should.Throw<ArgumentException>(() => builder.AddInt32(1, _ => captured!.WithTariff(1)));
        new MbusUserDataBuilder().AddNoData(record => captured = Volume(record));
        Should.Throw<ArgumentException>(() => builder.AddNoData(_ => captured!));

        builder.Build().ShouldBe(previous);
    }

    [Fact]
    public void UpdateUserData_AcceptsExpandedPrimitiveAndVifRepresentations()
    {
        var device = MbusDevice.Create(12345678, "GWF", 60, MbusDeviceType.WaterMeter);

        device.UpdateUserData(builder => builder
            .AddInt64(1234567890123, Volume)
            .AddReal32(21.5f, record => record.As(MbusValueType.FlowTemperature, MbusUnit.Celsius))
            .AddBcd12Digit(123456789012, record => record.AsFabricationNumber())
            .AddText("A", record => record.AsTextUnit("Label"))
            .AddBinary(new byte[] { 1, 2, 3 }, record => record.AsTextUnit("Payload"))
            .AddNoData(record => Volume(record).WithRecordError(MbusRecordError.NoDataAvailable)));
    }

    private static IMbusRecordWithUnitBuilder Volume(IMbusRecordWithValueBuilder record) =>
        record.AsCubicMetersExp0();
}
