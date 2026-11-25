namespace Metering.Mbus.Protocol.Frames.TransportLayer

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

module TplCodec =

    let parse: Parser<Field<TplRaw>> =
        parseField "TPL"
        <| parser {
            let! ci = CiFieldTpl.parse

            match ci with
            | CiFieldTpl.NoneHeaderField ci ->
                return! TplWithNoneHeaderRaw.parse ci |>> TplRaw.NoneHeader
            | CiFieldTpl.ShortHeaderField ci ->
                return! TplWithShortHeaderRaw.parse ci |>> TplRaw.ShortHeader
            | CiFieldTpl.LongHeaderField ci ->
                return! TplWithLongHeaderRaw.parse ci |>> TplRaw.LongHeader
        }

    let validate (raw: Field<TplRaw>) : Validation<Field<Tpl>> =

        match raw with
        | TplRaw.NoneHeaderField tpl ->
            TplWithNoneHeader.validate tpl |> map (Field.map Tpl.NoneHeader)
        | TplRaw.ShortHeaderField tpl ->
            TplWithShortHeader.validate tpl |> map (Field.map Tpl.ShortHeader)
        | TplRaw.LongHeaderField tpl ->
            TplWithLongHeader.validate tpl |> map (Field.map Tpl.LongHeader)

    let decode (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<Tpl>> =
        decoder {
            let! raw = Decoder.parse parse source
            return! Decoder.validate validate raw
        }
