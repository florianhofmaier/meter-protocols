using Metering.Common.Decoding.Parsers.Types;

namespace Metering.Common.Decoding.ByteReaders;

public static class ByteReaderFactory
{
    public static IByteReader Create(ReadOnlyMemory<byte> bytes) =>
        new ByteReader(bytes, 0);
}