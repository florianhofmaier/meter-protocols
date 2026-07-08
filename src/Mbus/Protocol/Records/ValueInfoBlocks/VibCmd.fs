namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.ValueInfoBlocks.VibCommon

module VibCmd =

    let fromRaw
        (raw: ParsedField<InfoBlockRaw>)
        : Validation<VibCmd> =

        validator {
            let bytes = vibBytes raw
            let first = bytes.Span[0]

            match first with
            | _ when isSpecial anyVif first ->
                return! failed raw "Any VIF (0x7E) is not supported"

            | _ when isSpecial mfrVif first ->
                return! failed raw "Manufacturer-specific VIF (0x7F) is not supported in SND_UD messages"

            | _ when isSpecial textVif first ->
                return! failed raw "Text VIF (0x7C) is not supported in SND_UD messages"

            | _ ->
                return!
                    validateNormal
                        ActionCode.tryMap
                        "command extension/action"
                        (fun vif extensions actions ->
                            VibCmd.Normal
                                {
                                    Def = vif
                                    Ext = extensions
                                    Actions = actions
                                })
                        raw
        }

