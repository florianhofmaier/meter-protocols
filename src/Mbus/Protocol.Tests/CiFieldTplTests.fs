module Metering.Mbus.Protocol.Tests.CiFieldTplTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.Transport
open Metering.Mbus.Protocol.Tests.TestSupport

[<Fact>]
let ``all supported TPL CI cases round-trip through value and parse`` () =
    [
        0x50uy, NoneTplHeader ApplicationResetOrSelectNoHeader
        0x51uy, NoneTplHeader Command
        0x52uy, NoneTplHeader SelectionOfDevice
        0x53uy, LongTplHeader ApplicationResetOrSelectLongHeader
        0x57uy, ShortTplHeader ApplicationResetOrSelectShortHeader
        0x72uy, LongTplHeader ResponseLongHeader
        0x75uy, LongTplHeader AlarmLongHeader
        0x7Auy, ShortTplHeader ResponseShortHeader
    ]
    |> List.iter (fun (rawValue, expected) ->
        CiFieldTpl.value expected
        |> should equal rawValue

        parseExactly CiFieldTpl.parse [| rawValue |]
        |> fun parsed -> parsed.Value
        |> should equal expected)

[<Fact>]
let ``CI 0x57 parses as application reset or select with short TPL header`` () =
    let raw =
        parseExactly
            TplRaw.parse
            [|
                0x57uy
                0x08uy
                0x00uy
                0x00uy; 0x00uy
            |]

    match raw.Value with
    | TplRaw.ShortHeader {
        Ci = { Value = ApplicationResetOrSelectShortHeader }
        Header = { Value = ShortHeaderRaw.Mode0Raw _ }
      } -> ()

    | actual ->
        failwith $"Expected CI 0x57 with a Mode 0 short TPL header, got %A{actual}"

[<Fact>]
let ``every raw CI byte is covered by the selected normative tables`` () =
    [ 0 .. 255 ]
    |> List.iter (fun value ->
        match
            runExactly
                (reader [| byte value |])
                trace
                CiFieldTpl.parse
        with
        | Ok _
        | Error _ ->
            ())
