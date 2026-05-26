namespace Metering.Common.Parsers

open Metering.Common.Parsers.ParserTree

type ParserError =
    {
        Node : ParsedNode
        Pos : int
        Msg : string
    }

// module ParserError =
//     let toString (e: ParserError) : string =
//         let ctxStr =
//             match ParserContext.toDisplayPath e.Source.Ctx with
//             | "" -> ""
//             | path -> $" Context: {path}"
//
//         let span = e.Source.Span
//
//         let location =
//             if span.Length = 0 then
//                 $"at position {span.Offset}"
//             else
//                 $"at position {span.Offset}, length {span.Length}"
//
//         $"Error while parsing{ctxStr} {location}: {e.Msg}"


module ErrorHandling =

    let inline err st msg =
        Error {
            Node = st.Current
            Pos = ParserState.absolutePos st
            Msg = msg
        }

    let inline errBefore st n msg =
        Error {
            Node = st.Current
            Pos = ParserState.absolutePos st - n
            Msg = msg
        }

    let inline errBufOverflow st =
        err st "unexpected end of buffer"

