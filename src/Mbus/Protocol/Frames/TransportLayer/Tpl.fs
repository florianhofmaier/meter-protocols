namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames

type TplRaw =
    | NoneHeader of TplWithNoneHeaderRaw
    | ShortHeader of TplWithShortHeaderRaw
    | LongHeader of TplWithLongHeaderRaw

module TplRaw =

    let (|NoneHeaderField|ShortHeaderField|LongHeaderField|)
        (field: Field<TplRaw>) =

        match field.Value with
        | TplRaw.NoneHeader tpl ->
            NoneHeaderField (field |> Field.withValue tpl)
        | TplRaw.ShortHeader tpl ->
            ShortHeaderField (field |> Field.withValue tpl)
        | TplRaw.LongHeader tpl ->
            LongHeaderField (field |> Field.withValue tpl)

    let ci =
        function
        | NoneHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.NoneHeader
        | ShortHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.ShortHeader
        | LongHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.LongHeader

    let aplData =
        function
        | NoneHeader tpl -> tpl.AplData
        | ShortHeader tpl -> tpl.AplData
        | LongHeader tpl -> tpl.AplData

type Tpl =
    | NoneHeader of TplWithNoneHeader
    | ShortHeader of TplWithShortHeader
    | LongHeader of TplWithLongHeader

module Tpl =

    let ci =
        function
        | NoneHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.NoneHeader
        | ShortHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.ShortHeader
        | LongHeader tpl -> tpl.Ci |> Field.map CiFieldTpl.LongHeader

    let aplData =
        function
        | NoneHeader tpl -> tpl.AplData
        | ShortHeader tpl -> tpl.AplData
        | LongHeader tpl -> tpl.AplData
