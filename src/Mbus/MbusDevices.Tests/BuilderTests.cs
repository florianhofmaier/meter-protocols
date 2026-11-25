using Shouldly;

namespace Metering.Mbus.MbusDevices.Tests;

public class BuilderTests
{
    [Fact]
    public void Build_WithoutRecordsReturnsEmptyApplicationData()
    {
        IMbusUserDataBuilder builder = new MbusUserDataBuilder();

        builder.Build().ShouldBeEmpty();
    }

    [Fact]
    public void Build_EncodesRecordsInOrderWithoutFraming()
    {
        // EN 13757-3:2025, Tables 4/8/10: Int32, DIFE fields, volume and fabrication number.
        var builder = new MbusUserDataBuilder()
            .AddInt32(1234, record => record.AsCubicMetersExpMinus3()
                .WithStorageNumber(35).WithTariff(5).WithSubUnit(3)
                .WithFunctionField(MbusFunctionField.MaxValue))
            .AddInt32(-1234, record => record.AsFabricationNumber());

        builder.Build().ShouldBe(Convert.FromHexString("D4D15113D204000004782EFBFFFF"));
    }

    [Fact]
    public void Build_DoesNotLimitTheCompleteDataSetToOneTelegram()
    {
        // EN 13757-3:2025, 6.1-6.2: application data contains a sequence of complete records.
        var record = Convert.FromHexString("0413D2040000");
        var builder = new MbusUserDataBuilder();
        for (var i = 0; i < 100; i++)
            builder.AddInt32(1234, value => value.AsCubicMetersExpMinus3());

        builder.Build().ShouldBe(Enumerable.Range(0, 100).SelectMany(_ => record).ToArray());
    }

    [Fact]
    public void Build_ReturnsIndependentOutputAndPreservesEarlierResults()
    {
        var builder = new MbusUserDataBuilder()
            .AddInt32(1234, record => record.AsCubicMetersExpMinus3());
        var expected = Convert.FromHexString("0413D2040000");
        var first = builder.Build();
        var second = builder.Build();

        first.ShouldNotBeSameAs(second);
        first[0] = 0;
        second.ShouldBe(expected);
        builder.Build().ShouldBe(expected);

        builder.AddInt32(1, record => record.AsFabricationNumber());

        second.ShouldBe(expected);
        builder.Build().ShouldBe(expected.Concat(Convert.FromHexString("047801000000")).ToArray());
    }
}
