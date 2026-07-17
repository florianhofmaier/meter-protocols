namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.ValueInfoBlocks.VibCommon

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VibRsp =

    let fromRaw
        (raw: Field<InfoBlockRaw>)
        : Validation<VibRsp> =

        validator {
            let bytes = vibBytes raw
            let first = bytes.Span[0]

            match first with
            | _ when isSpecial anyVif first ->
                return! failed raw "Any VIF (0x7E) is not supported"

            | _ when isSpecial mfrVif first ->
                return VibRsp.Mfr bytes

            | _ when isSpecial textVif first ->
                let! text = TextVib.fromRaw raw
                return VibRsp.Text text

            | _ ->
                return!
                    validateNormal
                        RecordError.tryMap
                        "response extension"
                        (fun vif extensions errors ->
                            VibRsp.Normal
                                {
                                    Def = vif
                                    Ext = extensions
                                    Codes = errors
                                })
                        raw
        }
