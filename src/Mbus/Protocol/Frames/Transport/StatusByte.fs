namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type StatusByteRaw =
    private Status of uint8

module StatusByteRaw =

    let value (Status v) =
        v

    let parse : Parser<ParsedField<StatusByteRaw>> =
        parseField "Status" parseU8
        |>> ParsedField.map Status

type ApplicationError =
    | NoError
    | Busy
    | AnyError
    | AbnormalCondition

module ApplicationError =

    let fromByte b=
        let mask b = b &&& 0x03uy
        match mask b with
        | 0x00uy -> NoError
        | 0x01uy -> Busy
        | 0x02uy -> AnyError
        | 0x03uy -> AbnormalCondition
        | _ -> failwith "Unreachable"

type StatusByte =
    {
        ApplicationError : ApplicationError
        PowerLow : bool
        PermanentError : bool
        TemporaryError : bool
        Bit5 : bool
        Bit6 : bool
        Bit7 : bool
    }

module StatusByte =

    let fromRaw
        (raw: ParsedField<StatusByteRaw>)
        : Validation<StatusByte> =

        let value = StatusByteRaw.value raw.Value

        let powerLow = (value &&& 0b0000_0100uy) <> 0uy
        let permanentError = (value &&& 0b0000_1000uy) <> 0uy
        let temporaryError = (value &&& 0b0001_0000uy) <> 0uy
        let bit5 = (value &&& 0b0010_0000uy) <> 0uy
        let bit6 = (value &&& 0b0100_0000uy) <> 0uy
        let bit7 = (value &&& 0b1000_0000uy) <> 0uy

        passed {
            ApplicationError = ApplicationError.fromByte value
            PowerLow = powerLow
            PermanentError = permanentError
            TemporaryError = temporaryError
            Bit5 = bit5
            Bit6 = bit6
            Bit7 = bit7
        }
