using Metering.Common.Decoding.Parsers.Types;
using Xunit;

namespace Metering.Common.Decoding.ByteReaders.Tests;

public class ByteReaderTests
{
    [Fact]
    public void InitialStateExposesSuppliedBoundedMemory()
    {
        var memory = new ReadOnlyMemory<byte>(new byte[] { 0x00, 0xAA, 0xBB, 0xCC }, 1, 2);
        var reader = new ByteReader(memory, 20);

        Assert.Equal(new byte[] { 0xAA, 0xBB }, reader.Buffer.ToArray());
        Assert.Equal(20, reader.Position);
        Assert.Equal(2, reader.Remaining);
    }

    [Fact]
    public void ReadZeroReturnsEmptyMemoryAndDoesNotAdvance()
    {
        var reader = new ByteReader(new byte[] { 0xAA }, 10);

        var bytes = reader.Read(0);

        Assert.Empty(bytes.ToArray());
        Assert.Equal(10, reader.Position);
        Assert.Equal(1, reader.Remaining);
    }

    [Fact]
    public void SequentialReadsAdvanceAbsolutePositionAndRemainingCount()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB, 0xCC }, 5);

        Assert.Equal(new byte[] { 0xAA, 0xBB }, reader.Read(2).ToArray());
        Assert.Equal(7, reader.Position);
        Assert.Equal(1, reader.Remaining);
        Assert.Equal(new byte[] { 0xCC }, reader.Read(1).ToArray());
        Assert.Equal(8, reader.Position);
        Assert.Equal(0, reader.Remaining);
    }

    [Fact]
    public void ReadExactlyToEndSucceeds()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 3);

        Assert.Equal(new byte[] { 0xAA, 0xBB }, reader.Read(2).ToArray());
        Assert.Equal(5, reader.Position);
        Assert.Equal(0, reader.Remaining);
    }

    [Theory]
    [InlineData(2, 11, "Requested 2 byte(s), remaining 1")]
    [InlineData(-1, 11, "Requested -1 byte(s), remaining 1")]
    public void ReadInvalidCountsThrowStructuredParserFailure(int count, int expectedPosition, string expectedMessagePart)
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 10);
        reader.Read(1);

        var ex = Assert.Throws<ParserException>(() => reader.Read(count));

        Assert.Equal(new SourceId(-1), ex.Error.Source);
        Assert.Equal(expectedPosition, ex.Error.Pos);
        Assert.Contains(expectedMessagePart, ex.Error.Msg);
        Assert.Equal(11, reader.Position);
        Assert.Equal(1, reader.Remaining);
    }

    [Fact]
    public void PeekReturnsBytesWithoutChangingState()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 7);

        Assert.Equal(new byte[] { 0xAA }, reader.Peek(1).ToArray());
        Assert.Equal(7, reader.Position);
        Assert.Equal(2, reader.Remaining);
        Assert.Empty(reader.Peek(0).ToArray());
        Assert.Equal(7, reader.Position);
    }

    [Theory]
    [InlineData(3)]
    [InlineData(-1)]
    public void PeekInvalidCountsThrowStructuredParserFailure(int count)
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 7);

        var ex = Assert.Throws<ParserException>(() => reader.Peek(count));

        Assert.Equal(new SourceId(-1), ex.Error.Source);
        Assert.Equal(7, ex.Error.Pos);
        Assert.Contains($"Requested {count} byte(s), remaining 2", ex.Error.Msg);
        Assert.Equal(7, reader.Position);
        Assert.Equal(2, reader.Remaining);
    }

    [Fact]
    public void SkipAdvancesPositionAndSupportsZeroAndExactEnd()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 30);

        reader.Skip(0);
        Assert.Equal(30, reader.Position);
        reader.Skip(1);
        Assert.Equal(31, reader.Position);
        Assert.Equal(1, reader.Remaining);
        reader.Skip(1);
        Assert.Equal(32, reader.Position);
        Assert.Equal(0, reader.Remaining);
    }

    [Theory]
    [InlineData(3)]
    [InlineData(-1)]
    public void SkipInvalidCountsThrowStructuredParserFailure(int count)
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 30);

        var ex = Assert.Throws<ParserException>(() => reader.Skip(count));

        Assert.Equal(new SourceId(-1), ex.Error.Source);
        Assert.Equal(30, ex.Error.Pos);
        Assert.Contains($"Requested {count} byte(s), remaining 2", ex.Error.Msg);
    }

    [Fact]
    public void SliceCreatesBoundedIndependentReaderAtCurrentAbsolutePosition()
    {
        var parent = new ByteReader(new byte[] { 0xAA, 0xBB, 0xCC, 0xDD }, 100);
        parent.Read(1);

        var child = parent.Slice(2);

        Assert.Equal(new byte[] { 0xBB, 0xCC }, child.Buffer.ToArray());
        Assert.Equal(101, child.Position);
        Assert.Equal(2, child.Remaining);

        Assert.Equal(new byte[] { 0xBB }, child.Read(1).ToArray());
        Assert.Equal(102, child.Position);
        Assert.Equal(101, parent.Position);

        parent.Read(1);
        Assert.Equal(102, parent.Position);
        Assert.Equal(102, child.Position);
        Assert.Equal(1, child.Remaining);
    }

    [Fact]
    public void NestedSlicesPreserveAbsolutePositions()
    {
        var parent = new ByteReader(new byte[] { 0xAA, 0xBB, 0xCC }, 50);
        parent.Skip(1);

        var child = parent.Slice(2);
        child.Skip(1);
        var grandchild = child.Slice(1);

        Assert.Equal(new byte[] { 0xCC }, grandchild.Buffer.ToArray());
        Assert.Equal(52, grandchild.Position);
        Assert.Equal(1, grandchild.Remaining);
    }

    [Fact]
    public void ZeroLengthSliceWorks()
    {
        var parent = new ByteReader(new byte[] { 0xAA }, 8);

        var child = parent.Slice(0);

        Assert.Empty(child.Buffer.ToArray());
        Assert.Equal(8, child.Position);
        Assert.Equal(0, child.Remaining);
        Assert.Equal(8, parent.Position);
    }

    [Theory]
    [InlineData(2)]
    [InlineData(-1)]
    public void SliceInvalidCountsThrowStructuredParserFailure(int count)
    {
        var reader = new ByteReader(new byte[] { 0xAA }, 8);

        var ex = Assert.Throws<ParserException>(() => reader.Slice(count));

        Assert.Equal(new SourceId(-1), ex.Error.Source);
        Assert.Equal(8, ex.Error.Pos);
        Assert.Contains($"Requested {count} byte(s), remaining 1", ex.Error.Msg);
    }
}
