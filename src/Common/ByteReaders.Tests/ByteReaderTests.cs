using Metering.Common.Decoding.Parsers.Types;
using Xunit;

namespace Metering.Common.Decoding.ByteReaders.Tests;

public class ByteReaderTests
{
    private static T Success<T>(Microsoft.FSharp.Core.FSharpResult<T, ReaderError> result)
    {
        Assert.True(result.IsOk);
        return result.ResultValue;
    }

    private static ReaderError Failure<T>(Microsoft.FSharp.Core.FSharpResult<T, ReaderError> result)
    {
        Assert.True(result.IsError);
        return result.ErrorValue;
    }

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

        var bytes = Success(reader.Read(0));

        Assert.Empty(bytes.ToArray());
        Assert.Equal(10, reader.Position);
        Assert.Equal(1, reader.Remaining);
    }

    [Fact]
    public void SequentialReadsAdvanceAbsolutePositionAndRemainingCount()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB, 0xCC }, 5);

        Assert.Equal(new byte[] { 0xAA, 0xBB }, Success(reader.Read(2)).ToArray());
        Assert.Equal(7, reader.Position);
        Assert.Equal(1, reader.Remaining);
        Assert.Equal(new byte[] { 0xCC }, Success(reader.Read(1)).ToArray());
        Assert.Equal(8, reader.Position);
        Assert.Equal(0, reader.Remaining);
    }

    [Fact]
    public void ReadExactlyToEndSucceeds()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 3);

        Assert.Equal(new byte[] { 0xAA, 0xBB }, Success(reader.Read(2)).ToArray());
        Assert.Equal(5, reader.Position);
        Assert.Equal(0, reader.Remaining);
    }

    [Theory]
    [InlineData(2, 11, "Requested 2 byte(s), remaining 1")]
    [InlineData(-1, 11, "Requested -1 byte(s), remaining 1")]
    public void ReadInvalidCountsReturnStructuredFailure(int count, int expectedPosition, string expectedMessagePart)
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 10);
        Success(reader.Read(1));

        var error = Failure(reader.Read(count));

        Assert.Equal(expectedPosition, error.Pos);
        Assert.Contains(expectedMessagePart, error.Msg);
        Assert.Equal(11, reader.Position);
        Assert.Equal(1, reader.Remaining);
    }

    [Fact]
    public void PeekReturnsBytesWithoutChangingState()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 7);

        Assert.Equal(new byte[] { 0xAA }, Success(reader.Peek(1)).ToArray());
        Assert.Equal(7, reader.Position);
        Assert.Equal(2, reader.Remaining);
        Assert.Empty(Success(reader.Peek(0)).ToArray());
        Assert.Equal(7, reader.Position);
    }

    [Theory]
    [InlineData(3)]
    [InlineData(-1)]
    public void PeekInvalidCountsReturnStructuredFailure(int count)
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 7);

        var error = Failure(reader.Peek(count));

        Assert.Equal(7, error.Pos);
        Assert.Contains($"Requested {count} byte(s), remaining 2", error.Msg);
        Assert.Equal(7, reader.Position);
        Assert.Equal(2, reader.Remaining);
    }

    [Fact]
    public void SkipAdvancesPositionAndSupportsZeroAndExactEnd()
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 30);

        Success(reader.Skip(0));
        Assert.Equal(30, reader.Position);
        Success(reader.Skip(1));
        Assert.Equal(31, reader.Position);
        Assert.Equal(1, reader.Remaining);
        Success(reader.Skip(1));
        Assert.Equal(32, reader.Position);
        Assert.Equal(0, reader.Remaining);
    }

    [Theory]
    [InlineData(3)]
    [InlineData(-1)]
    public void SkipInvalidCountsReturnStructuredFailure(int count)
    {
        var reader = new ByteReader(new byte[] { 0xAA, 0xBB }, 30);

        var error = Failure(reader.Skip(count));

        Assert.Equal(30, error.Pos);
        Assert.Contains($"Requested {count} byte(s), remaining 2", error.Msg);
    }

    [Fact]
    public void SliceCreatesBoundedIndependentReaderAtCurrentAbsolutePosition()
    {
        var parent = new ByteReader(new byte[] { 0xAA, 0xBB, 0xCC, 0xDD }, 100);
        Success(parent.Read(1));

        var child = Success(parent.Slice(2));

        Assert.Equal(new byte[] { 0xBB, 0xCC }, child.Buffer.ToArray());
        Assert.Equal(101, child.Position);
        Assert.Equal(2, child.Remaining);

        Assert.Equal(new byte[] { 0xBB }, Success(child.Read(1)).ToArray());
        Assert.Equal(102, child.Position);
        Assert.Equal(101, parent.Position);

        Success(parent.Read(1));
        Assert.Equal(102, parent.Position);
        Assert.Equal(102, child.Position);
        Assert.Equal(1, child.Remaining);
    }

    [Fact]
    public void NestedSlicesPreserveAbsolutePositions()
    {
        var parent = new ByteReader(new byte[] { 0xAA, 0xBB, 0xCC }, 50);
        Success(parent.Skip(1));

        var child = Success(parent.Slice(2));
        Success(child.Skip(1));
        var grandchild = Success(child.Slice(1));

        Assert.Equal(new byte[] { 0xCC }, grandchild.Buffer.ToArray());
        Assert.Equal(52, grandchild.Position);
        Assert.Equal(1, grandchild.Remaining);
    }

    [Fact]
    public void ZeroLengthSliceWorks()
    {
        var parent = new ByteReader(new byte[] { 0xAA }, 8);

        var child = Success(parent.Slice(0));

        Assert.Empty(child.Buffer.ToArray());
        Assert.Equal(8, child.Position);
        Assert.Equal(0, child.Remaining);
        Assert.Equal(8, parent.Position);
    }

    [Theory]
    [InlineData(2)]
    [InlineData(-1)]
    public void SliceInvalidCountsReturnStructuredFailure(int count)
    {
        var reader = new ByteReader(new byte[] { 0xAA }, 8);

        var error = Failure(reader.Slice(count));

        Assert.Equal(8, error.Pos);
        Assert.Contains($"Requested {count} byte(s), remaining 1", error.Msg);
    }
}
