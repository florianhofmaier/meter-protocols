using Metering.Common.Decoding.Parsers.Types;

namespace Metering.Common.Decoding.ByteReaders;

/// <summary>Creates byte readers over supplied memory without copying it.</summary>
public static class ByteReaderFactory
{
    /// <summary>Creates a reader whose buffer starts at absolute position zero.</summary>
    public static IByteReader Create(ReadOnlyMemory<byte> bytes) =>
        new ByteReader(bytes, 0);

    /// <summary>Creates a reader whose buffer starts at the supplied absolute position.</summary>
    public static IByteReader Create(ReadOnlyMemory<byte> bytes, int offset) =>
        new ByteReader(bytes, offset);
}
