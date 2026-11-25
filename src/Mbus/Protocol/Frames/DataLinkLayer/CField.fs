namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.BaseWriters.BinaryWriters
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type CFieldRaw = private CField of uint8

module CFieldRaw =

    let value (CField v) = v

    let parse: Parser<Field<CFieldRaw>> =
        parseField "CField" parseU8 |>> Field.map CField

type PrimaryFunction =
    | LinkReset
    | SendUserData
    | RequestUserDataClass1
    | RequestUserDataClass2
    | SendUserDataNoResponse

module PrimaryFunction =

    let validate raw : Validation<Field<PrimaryFunction>> =
        let b = CFieldRaw.value raw.Value

        match b &&& 0x0Fuy with
        | 0x00uy -> raw |> Field.withValue LinkReset |> passed
        | 0x04uy -> raw |> Field.withValue SendUserDataNoResponse |> passed
        | 0x03uy -> raw |> Field.withValue SendUserData |> passed
        | 0x0Auy -> raw |> Field.withValue RequestUserDataClass1 |> passed
        | 0x0Buy -> raw |> Field.withValue RequestUserDataClass2 |> passed
        | _ -> failed raw $"Invalid function code in CField with PRM=1: {b:X2}"

type PrimaryCField = {
    Fcb: bool
    Fcv: bool
    Func: Field<PrimaryFunction>
}

type SecondaryFunction = | ResponseUserData

module SecondaryFunction =

    let validate raw : Validation<Field<SecondaryFunction>> =
        let b = CFieldRaw.value raw.Value

        match b &&& 0x0Fuy with
        | 0x08uy -> raw |> Field.withValue ResponseUserData |> passed
        | _ -> failed raw $"Invalid function code in CField with PRM=0: {b:X2}"

type SecondaryCField = {
    Acd: bool
    Dfc: bool
    Func: Field<SecondaryFunction>
}

type CField =
    | Primary of PrimaryCField
    | Secondary of SecondaryCField

module CField =

    type private Originator =
        | PrimaryStation
        | SecondaryStation

    let private maskReservedBit = 0x80uy
    let private maskPrm = 0x40uy
    let private maskFcbAcd = 0x20uy
    let private maskFcvDfc = 0x10uy

    let private mapOriginator b =
        match b &&& maskPrm = maskPrm with
        | true -> PrimaryStation
        | false -> SecondaryStation

    let private mapFcbAcd b = b &&& maskFcbAcd = maskFcbAcd

    let private mapFcvDfc b = b &&& maskFcvDfc = maskFcvDfc

    let private validateReservedBit raw =
        validator {
            let b = CFieldRaw.value raw.Value

            if b &&& maskReservedBit = maskReservedBit then
                return! warning raw "Reserved bit must be 0"
            else
                return! passed ()
        }

    let private validatePrimary raw fcbAcd fcvDfc =
        validator {
            let! () = validateReservedBit raw
            and! func = PrimaryFunction.validate raw

            return
                Primary {
                    Fcb = fcbAcd
                    Fcv = fcvDfc
                    Func = func
                }
        }

    let private validateSecondary raw fcbAcd fcvDfc =
        validator {
            let! () = validateReservedBit raw
            and! func = SecondaryFunction.validate raw

            return
                Secondary {
                    Acd = fcbAcd
                    Dfc = fcvDfc
                    Func = func
                }
        }

    let validate (raw: Field<CFieldRaw>) : Validation<CField> =

        validator {
            let b = CFieldRaw.value raw.Value

            let fcbAcd = mapFcbAcd b
            let fcvDfc = mapFcvDfc b

            match mapOriginator b with
            | PrimaryStation -> return! validatePrimary raw fcbAcd fcvDfc

            | SecondaryStation -> return! validateSecondary raw fcbAcd fcvDfc
        }

    /// Writes a primary control field without requiring decoded provenance.
    let writePrimary fn fcb fcv =
        let code =
            match fn with
            | PrimaryFunction.LinkReset -> 0uy
            | PrimaryFunction.SendUserData -> 3uy
            | PrimaryFunction.SendUserDataNoResponse -> 4uy
            | PrimaryFunction.RequestUserDataClass1 -> 10uy
            | PrimaryFunction.RequestUserDataClass2 -> 11uy

        writeU8 (
            0x40uy
            ||| code
            ||| (if fcb then 0x20uy else 0uy)
            ||| (if fcv then 0x10uy else 0uy)
        )

    /// Writes the supported secondary control function and its flags.
    let writeSecondary acd dfc =
        writeU8 (8uy ||| (if acd then 0x20uy else 0uy) ||| (if dfc then 0x10uy else 0uy))

    /// Writes the modeled C-field bits, with the reserved bit cleared.
    let write =
        function
        | CField.Primary primary -> writePrimary primary.Func.Value primary.Fcb primary.Fcv
        | CField.Secondary secondary -> writeSecondary secondary.Acd secondary.Dfc
