namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open System
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type DedicatedKey =
    private
        DedicatedKey of ReadOnlyMemory<byte>

module DedicatedKey =
    let create bytes =
        DedicatedKey bytes

    let bytes (DedicatedKey bytes) =
        bytes

    let validate (value : Axdr.OctetString) =
        value
        |> Axdr.OctetString.toBytes
        |> create
        |> Validation.ok