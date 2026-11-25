using Microsoft.FSharp.Core;
using Metering.Common.Decoding.Parsers.Types;

namespace Metering.Common.Decoding.ByteReaders;

/// <summary>Reads views of a fixed byte buffer while tracking an absolute byte position.</summary>
/// <param name="buffer">The backing bytes, retained without copying.</param>
/// <param name="offset">The absolute position represented by the start of the buffer.</param>
public class ByteReader(ReadOnlyMemory<byte> buffer, int offset) : IByteReader
{
    private int _position;

    /// <summary>Gets the complete backing buffer without copying it or advancing the reader.</summary>
    public ReadOnlyMemory<byte> Buffer => buffer;

    /// <summary>Gets the current absolute byte position, including the initial offset.</summary>
    public int Position => offset + _position;

    /// <summary>Gets the number of unread bytes remaining in the buffer.</summary>
    public int Remaining => buffer.Length - _position;

    private ReaderError? ValidateCount(int count)
    {
        if ((uint)count > (uint)Remaining)
            return new ReaderError(
                Position,
                $"Unexpected end of buffer. Requested {count} byte(s), remaining {Remaining}.");

        return null;
    }

    /// <summary>Returns a view of the requested bytes and advances on success; negative or excessive counts return an error without advancing.</summary>
    public FSharpResult<ReadOnlyMemory<byte>, ReaderError> Read(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewError(error);

        var slice = buffer.Slice(_position, count);
        _position += count;

        return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewOk(slice);
    }

    /// <summary>Returns a view of the requested upcoming bytes without advancing; negative or excessive counts return an error.</summary>
    public FSharpResult<ReadOnlyMemory<byte>, ReaderError> Peek(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewError(error);

        return FSharpResult<ReadOnlyMemory<byte>, ReaderError>.NewOk(
            buffer.Slice(_position, count));
    }

    /// <summary>Creates an independent reader over the requested upcoming bytes at the current absolute position without advancing this reader; invalid counts return an error.</summary>
    public FSharpResult<IByteReader, ReaderError> Slice(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<IByteReader, ReaderError>.NewError(error);

        return FSharpResult<IByteReader, ReaderError>.NewOk(
            new ByteReader(buffer.Slice(_position, count), Position));
    }

    /// <summary>Advances by the requested byte count on success; negative or excessive counts return an error without advancing.</summary>
    public FSharpResult<Unit, ReaderError> Skip(int count)
    {
        var error = ValidateCount(count);
        if (error is not null)
            return FSharpResult<Unit, ReaderError>.NewError(error);

        _position += count;
        return FSharpResult<Unit, ReaderError>.NewOk(null!);
    }
}
