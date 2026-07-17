namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type CFieldRaw =
    private CField of uint8

module CFieldRaw =

    let value (CField v) =
        v

    let parse : Parser<ParsedField<CFieldRaw>> =
        parseField "CField" parseU8
        |>> ParsedField.map CField

type PrimaryFunction =
    | LinkReset
    | SendUserData
    | RequestUserDataClass1
    | RequestUserDataClass2
    | SendUserDataNoResponse

module PrimaryFunction =

    let validate raw =
        let b = CFieldRaw.value raw.Value

        match b &&& 0x0Fuy with
        | 0x00uy -> passed LinkReset
        | 0x04uy -> passed SendUserDataNoResponse
        | 0x03uy -> passed SendUserData
        | 0x0Auy -> passed RequestUserDataClass1
        | 0x0Buy -> passed RequestUserDataClass2
        | _ -> failed raw $"Invalid function code in CField with PRM=1: {b:X2}"

type PrimaryCField =
    {
        Fcb: bool
        Fcv: bool
        Func: PrimaryFunction
    }

type SecondaryFunction =
    | ResponseUserData

module SecondaryFunction =

    let validate raw =
        let b = CFieldRaw.value raw.Value

        match b &&& 0x0Fuy with
        | 0x00uy -> passed ResponseUserData
        | _ -> failed raw $"Invalid function code in CField with PRM=0: {b:X2}"

type SecondaryCField =
    {
        Acd: bool
        Dfc: bool
        Func: SecondaryFunction
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

    let private mapFcbAcd b =
        b &&& maskFcbAcd = maskFcbAcd

    let private mapFcvDfc b =
        b &&& maskFcvDfc = maskFcvDfc

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

    let fromRaw
        (raw: ParsedField<CFieldRaw>)
        : Validation<CField> =

        validator {
            let b = CFieldRaw.value raw.Value

            let fcbAcd = mapFcbAcd b
            let fcvDfc = mapFcvDfc b

            match mapOriginator b with
            | PrimaryStation ->
                return! validatePrimary raw fcbAcd fcvDfc

            | SecondaryStation ->
                return! validateSecondary raw fcbAcd fcvDfc
        }