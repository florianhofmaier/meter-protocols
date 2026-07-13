module Devices.Tests.DeviceSelectionTests

open Mbus
open Mbus.Devices
open Xunit
open FsUnit.Xunit

let createAddress id mfr version deviceType =
    match MbusAddress.create id mfr version deviceType with
    | Ok addr -> addr
    | Error msg -> failwith $"Failed to create address: {msg}"

[<Fact>]
let ``isSelected when all bytes match exactly should return true`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID does not match should return false`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0x79uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when manufacturer does not match should return false`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE7uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when version does not match should return false`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Duy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when device type does not match should return false`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x04uy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when ID has wildcard in low nibble of first byte should match`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0x7Fuy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID has wildcard in high nibble of first byte should match`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0xF8uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID has both nibbles wildcarded should match`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0xFFuy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID has all bytes wildcarded should match`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xE6uy; 0x1Euy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID starts with 5 and rest wildcarded should match ID 50000000`` () =
    let address = createAddress 50000000 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0x5Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID starts with 5 and rest wildcarded should match ID 59999999`` () =
    let address = createAddress 59999999 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0x5Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID starts with 5 and rest wildcarded should not match ID 49999999`` () =
    let address = createAddress 49999999 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0x5Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when ID starts with 5 and rest wildcarded should not match ID 60000000`` () =
    let address = createAddress 60000000 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0x5Fuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when ID matches pattern 123456XX should match ID 12345678`` () =
    let address = createAddress 12345678 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0x56uy; 0x34uy; 0x12uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID matches pattern 123456XX should match ID 12345600`` () =
    let address = createAddress 12345600 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0x56uy; 0x34uy; 0x12uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID matches pattern 123456XX should not match ID 12345778`` () =
    let address = createAddress 12345778 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0x56uy; 0x34uy; 0x12uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when manufacturer is wildcarded should match any manufacturer`` () =
    let address = createAddress 12345678 "ABC" 60 MbusDeviceType.WaterMeter
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xFFuy; 0xFFuy; 0x3Cuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when version is wildcarded should match any version`` () =
    let address = createAddress 12345678 "GWF" 99 MbusDeviceType.WaterMeter
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0xFFuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when device type is wildcarded should match any device type`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.HeatMeter
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0xE6uy; 0x1Euy; 0x3Cuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when all fields wildcarded should match any device`` () =
    let address = createAddress 98765432 "XYZ" 123 MbusDeviceType.GasMeter
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when only manufacturer specified should match that manufacturer`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.WaterMeter
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xE6uy; 0x1Euy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when only manufacturer specified should not match different manufacturer`` () =
    let address = createAddress 12345678 "ABC" 60 MbusDeviceType.WaterMeter
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xE6uy; 0x1Euy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when only device type specified should match that device type`` () =
    let address = createAddress 12345678 "ABC" 60 MbusDeviceType.WaterMeter
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when only device type specified should not match different device type`` () =
    let address = createAddress 12345678 "ABC" 60 MbusDeviceType.HeatMeter
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected when manufacturer is ABC should match correctly`` () =
    let address = createAddress 12345678 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0x78uy; 0x56uy; 0x34uy; 0x12uy; 0x43uy; 0x04uy; 0x01uy; 0x00uy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when manufacturer is ZZZ should match correctly`` () =
    let address = createAddress 99999999 "ZZZ" 255 MbusDeviceType.Unknown
    let selection = [| 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0x5Auy; 0x6Buy; 0xFFuy; 0x0Fuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID is 0 should match correctly`` () =
    let address = createAddress 0 "ABC" 0 MbusDeviceType.Other
    let selection = [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when ID is 99999999 should match correctly`` () =
    let address = createAddress 99999999 "ABC" 0 MbusDeviceType.Other
    let selection = [| 0x99uy; 0x99uy; 0x99uy; 0x99uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when version is 0 should match correctly`` () =
    let address = createAddress 12345678 "ABC" 0 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x00uy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected when version is 255 should match correctly`` () =
    let address = createAddress 12345678 "ABC" 255 MbusDeviceType.Other
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected for all GWF water meters should not match GWF heat meters`` () =
    let address = createAddress 12345678 "GWF" 60 MbusDeviceType.HeatMeter
    let selection = [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xE6uy; 0x1Euy; 0xFFuy; 0x07uy |]
    DeviceSelection.isSelected address selection |> should be False

[<Fact>]
let ``isSelected with pattern 1234567X should match 12345670 through 12345679`` () =
    let selection = [| 0x7Fuy; 0x56uy; 0x34uy; 0x12uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    for i in 0..9 do
        let id = 12345670 + i
        let address = createAddress id "ABC" 1 MbusDeviceType.Other
        DeviceSelection.isSelected address selection |> should be True

[<Fact>]
let ``isSelected with pattern 1234567X should not match 12345669 or 12345680`` () =
    let address1 = createAddress 12345669 "ABC" 1 MbusDeviceType.Other
    let address2 = createAddress 12345680 "ABC" 1 MbusDeviceType.Other
    let selection = [| 0x7Fuy; 0x56uy; 0x45uy; 0x23uy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]

    DeviceSelection.isSelected address1 selection |> should be False
    DeviceSelection.isSelected address2 selection |> should be False

