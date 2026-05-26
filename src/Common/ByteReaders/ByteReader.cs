using Metering.Common.Decoding.Parsers.Types;

namespace Metering.Common.Decoding.ByteReaders;

public class ByteReader(ReadOnlyMemory<byte> buffer, int offset) : IByteReader
{
    private int _position;

    public ReadOnlyMemory<byte> Buffer => buffer;

    public int Position => offset + _position;

    public int Remaining => buffer.Length - _position;

    private void EnsureAvailable(int count)
    {
        if ((uint) count > (uint) Remaining)
            throw new ParserException(
                new ParserError(Position,
                $"Unexpected end of buffer. Requested {count} byte(s), remaining {Remaining}."));
    }

    public ReadOnlyMemory<byte> Read(int count)
    {
        EnsureAvailable(count);

        var slice = buffer.Slice(_position, count);
        _position += count;

        return slice;
    }

    public ReadOnlyMemory<byte> Peek(int count)
    {
        EnsureAvailable(count);

        return buffer.Slice(_position, count);
    }

    public IByteReader Slice(int count)
    {
        EnsureAvailable(count);

        return new ByteReader(buffer.Slice(_position, count), Position);
    }

    public void Skip(int count)
    {
        EnsureAvailable(count);
        _position += count;
    }
}