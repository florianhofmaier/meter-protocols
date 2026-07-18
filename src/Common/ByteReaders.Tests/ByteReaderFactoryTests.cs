using Metering.Common.Decoding.ByteReaders;
using Metering.Common.Decoding.Parsers.Types;
using Xunit;

namespace Metering.Common.Decoding.ByteReaders.Tests;

public class ByteReaderFactoryTests
{
    [Fact]
    public void DefaultFactoryCreatesReaderAtAbsolutePositionZero()
    {
        var bytes = new byte[] { 0x01, 0x02 };

        var reader = ByteReaderFactory.Create(bytes);

        Assert.Equal(0, reader.Position);
        Assert.Equal(bytes, reader.Buffer.ToArray());
        Assert.Equal(bytes.Length, reader.Remaining);
    }

    [Fact]
    public void OffsetFactoryPreservesNonZeroAbsoluteBaseOffset()
    {
        var reader = ByteReaderFactory.Create(new byte[] { 0x01, 0x02 }, 27);

        Assert.Equal(27, reader.Position);
        reader.Read(1);
        Assert.Equal(28, reader.Position);
    }

    [Fact]
    public void OverloadsConstructEquivalentReadersApartFromOffset()
    {
        var bytes = new byte[] { 0xAA, 0xBB };

        var defaultReader = ByteReaderFactory.Create(bytes);
        var offsetReader = ByteReaderFactory.Create(bytes, 10);

        Assert.Equal(defaultReader.Buffer.ToArray(), offsetReader.Buffer.ToArray());
        Assert.Equal(defaultReader.Remaining, offsetReader.Remaining);
        Assert.Equal(0, defaultReader.Position);
        Assert.Equal(10, offsetReader.Position);
    }

    [Fact]
    public void EachFactoryCallReturnsIndependentReaderInstance()
    {
        var bytes = new byte[] { 0xAA, 0xBB };
        var first = ByteReaderFactory.Create(bytes);
        var second = ByteReaderFactory.Create(bytes);

        first.Read(1);

        Assert.Equal(1, first.Position);
        Assert.Equal(0, second.Position);
        Assert.Equal(2, second.Remaining);
    }

    [Fact]
    public void ZeroLengthBuffersAreSupported()
    {
        var reader = ByteReaderFactory.Create(Array.Empty<byte>());

        Assert.Empty(reader.Buffer.ToArray());
        Assert.Equal(0, reader.Position);
        Assert.Equal(0, reader.Remaining);
        Assert.Empty(reader.Read(0).ToArray());
    }
}
