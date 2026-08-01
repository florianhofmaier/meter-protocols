module Metering.Mbus.Protocol.Tests.CiFieldTplTests

open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Tests.TestSupport

[<Fact>]
let ``all supported TPL CI cases round-trip through value and parse`` () =
    [
        0x50uy, CiFieldTpl.NoneHeader CiFieldTplNoneHeader.ApplicationResetOrSelect
        0x51uy, CiFieldTpl.NoneHeader CiFieldTplNoneHeader.Command
        0x53uy, CiFieldTpl.LongHeader CiFieldTplLongHeader.ApplicationResetOrSelect
        0x57uy, CiFieldTpl.ShortHeader CiFieldTplShortHeader.ApplicationResetOrSelect
        0x72uy, CiFieldTpl.LongHeader CiFieldTplLongHeader.Response
        0x75uy, CiFieldTpl.LongHeader CiFieldTplLongHeader.Alarm
        0x7Auy, CiFieldTpl.ShortHeader CiFieldTplShortHeader.Response
    ]
    |> List.iter (fun (rawValue, expected) ->
        CiFieldTpl.value expected |> should equal rawValue
        parseExactly CiFieldTpl.parse [| rawValue |]
        |> Field.value
        |> should equal expected)

[<Fact>]
let ``CI 0x52 remains typed as lower-layer device selection`` () =
    parseExactly CiField.parse [| 0x52uy |]
    |> Field.value
    |> should equal (
        CiField.LowerLayerManagement CiLowerLayerManagement.SelectionOfDevice
    )

[<Fact>]
let ``CI 0x57 parses as application reset or select with short TPL header`` () =
    let raw =
        parseExactly
            TplRaw.parse
            [| 0x57uy; 0x08uy; 0x00uy; 0x00uy; 0x00uy |]

    match raw.Value with
    | TplRaw.ShortHeader {
        Ci = { Value = CiFieldTplShortHeader.ApplicationResetOrSelect }
        Header = { Value = ShortHeaderRaw.Mode0Raw _ }
      } -> ()
    | actual ->
        failwith $"Expected CI 0x57 with a Mode 0 short TPL header, got %A{actual}"

[<Fact>]
let ``every raw CI byte is covered by the selected normative tables`` () =
    [ 0 .. 255 ]
    |> List.iter (fun value ->
        match parseResult CiFieldTpl.parse [| byte value |] with
        | Ok _ | Error _ -> ())
