namespace Metering.Mbus.Protocol.Security

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Decoders.Core.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.Transport

module AplProtection =

    let private issue
        (field: ParsedField<_>)
        (message: string)
        : Issue =

        {
            FieldId = field.Id
            Message = message
        }

    let private encryptionFailed field message =
        issue field message
        |> EncryptionFailed
        |> error

    let private aplBytes (tplAplData: ParsedField<AplDataRaw>) =
        tplAplData
        |> AplDataRaw.toByteField

    let unprotect
        (securityContext: SecurityContext)
        (tpl: Tpl)
        : Decoder<ParsedField<ReadOnlyMemory<byte>>> =

        decoder {
            match tpl with
            | Tpl.NoneHeader tpl ->
                return aplBytes tpl.AplData

            | Tpl.ShortHeader tpl ->
                match tpl.Header with
                | ShortHeader.Mode0 _ ->
                    return aplBytes tpl.AplData

                | ShortHeader.Mode5 header ->
                    match securityContext with
                    | SecurityContext.Mode5 mode5 ->
                        match mode5.MeterAddress with
                        | Some meterAddress ->
                            return!
                                Mode5.unprotect
                                    mode5.Key
                                    meterAddress
                                    header.Acc
                                    header.Cnf
                                    (aplBytes tpl.AplData)

                        | None ->
                            return!
                                encryptionFailed
                                    tpl.AplData
                                    "security mode 5 with short TPL header requires the meter address in the security context"

                    | SecurityContext.NoSecurity ->
                        return!
                            encryptionFailed
                                tpl.AplData
                                "security mode 5 requires a mode 5 security context"

            | Tpl.LongHeader tpl ->
                match tpl.Header with
                | LongHeader.Mode0 _ ->
                    return aplBytes tpl.AplData

                | LongHeader.Mode5 header ->
                    match securityContext with
                    | SecurityContext.Mode5 mode5 ->
                        return!
                            Mode5.unprotect
                                mode5.Key
                                header.Device
                                header.Acc
                                header.Cnf
                                (aplBytes tpl.AplData)

                    | SecurityContext.NoSecurity ->
                        return!
                            encryptionFailed
                                tpl.AplData
                                "security mode 5 requires a mode 5 security context"
        }
