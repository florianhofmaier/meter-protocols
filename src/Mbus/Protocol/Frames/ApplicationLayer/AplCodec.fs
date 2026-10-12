namespace Metering.Mbus.Protocol.Frames.ApplicationLayer

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames

module AplCodec =

    let parse (ci: CiFieldTpl) = AplRaw.parse ci

    let fromRaw (raw: Field<AplRaw>) : Validation<Field<Apl>> =
        validateField (Field.value >> Apl.fromRaw) raw

    let decode (ci: CiFieldTpl) (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<Apl>> =
        decoder {
            let! raw = Core.parse (parse ci) source
            return! Core.validate fromRaw raw
        }
