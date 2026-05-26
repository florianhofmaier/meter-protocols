namespace Metering.Common.Parsers

open System
open Metering.Common.Parsers.ParserTree

[<Struct>]
type ParserState =
    {
        Buf : ReadOnlyMemory<byte>
        Off : int
        BasePos : int
        Current : ParsedNode
    }

module ParserState =

    let init bytes rootName =
        let root = RootNode.init rootName

        {
            Buf = bytes
            Off = 0
            BasePos = 0
            Current = Root root
        }

    let absolutePos (st: ParserState) = st.BasePos + st.Off

    let fromParsedMemory
        (source: Parsed<ReadOnlyMemory<byte>>)
        : ParserState =

        {
            Buf = source.Value
            Off = 0
            BasePos = (ParsedNode.span source.Node).Offset
            Current = source.Node
        }