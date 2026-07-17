namespace Metering.Dlms.Protocol.Xdlms.InitiateRequest

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol
open Metering.Dlms.Protocol.Axdr

type DedicatedKey =
    private
        DedicatedKey of ReadOnlyMemory<byte>

module DedicatedKey =
    let create bytes =
        DedicatedKey bytes

    let bytes (DedicatedKey bytes) =
        bytes

    let validate
        (raw: Field<Axdr.OctetString>)
        : Validation<Field<DedicatedKey>> =

        validator {
            do!
                ensure
                    raw
                    "dedicated-key must not be empty"
                    (OctetString.length raw.Value > 0)

            return
                raw.Value
                |> OctetString.toBytes
                |> DedicatedKey
                |> fun value -> Field.withValue value raw
        }
