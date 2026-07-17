module Metering.Mbus.Protocol.Tests.ValueConverterTests

open Xunit
open FsUnit.Xunit
open Metering.Mbus.Protocol.Records.Values
open Metering.Mbus.Protocol.Tests.TestSupport

[<Fact>]
let ``Integer24Bit decodes little-endian positive value`` () =
    bytesField [| 0x01uy; 0x02uy; 0x03uy |]
    |> Integer24Bit.fromBytes
    |> validationValue
    |> should equal 0x030201

[<Fact>]
let ``Integer24Bit decodes positive maximum`` () =
    bytesField [| 0xFFuy; 0xFFuy; 0x7Fuy |]
    |> Integer24Bit.fromBytes
    |> validationValue
    |> should equal 8_388_607

[<Fact>]
let ``Integer24Bit sign-extends negative values`` () =
    bytesField [| 0xFEuy; 0xFFuy; 0xFFuy |]
    |> Integer24Bit.fromBytes
    |> validationValue
    |> should equal -2

[<Fact>]
let ``Integer24Bit decodes boundary adjacent negative value`` () =
    bytesField [| 0x01uy; 0x00uy; 0x80uy |]
    |> Integer24Bit.fromBytes
    |> validationValue
    |> should equal -8_388_607

[<Fact>]
let ``Integer48Bit decodes little-endian positive value`` () =
    bytesField [| 0x01uy; 0x02uy; 0x03uy; 0x04uy; 0x05uy; 0x06uy |]
    |> Integer48Bit.fromBytes
    |> validationValue
    |> should equal 0x060504030201L

[<Fact>]
let ``Integer48Bit decodes positive maximum`` () =
    bytesField [| 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0x7Fuy |]
    |> Integer48Bit.fromBytes
    |> validationValue
    |> should equal 140_737_488_355_327L

[<Fact>]
let ``Integer48Bit sign-extends negative values`` () =
    bytesField [| 0xFEuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy; 0xFFuy |]
    |> Integer48Bit.fromBytes
    |> validationValue
    |> should equal -2L

[<Fact>]
let ``Integer48Bit decodes boundary adjacent negative value`` () =
    bytesField [| 0x01uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x80uy |]
    |> Integer48Bit.fromBytes
    |> validationValue
    |> should equal -140_737_488_355_327L

