namespace Metering.Mbus.Protocol.Frames.Transport

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type ConfigurationFieldBitsRaw =
    private ConfigurationFieldBits of uint16

type ConfigurationFieldRaw =
    | Mode0Raw of ParsedField<ConfigurationFieldBitsRaw>
    | Mode5Raw of ParsedField<ConfigurationFieldBitsRaw>

module ConfigurationFieldBitsRaw =

    let value (ConfigurationFieldBits v) = v

    let parse : Parser<ParsedField<ConfigurationFieldBitsRaw>> =
        parseField
            "Configuration Field"
            parseU16LittleEndian
        |>> ParsedField.map ConfigurationFieldBits

module ConfigurationFieldRaw =

    let bits = function
        | Mode0Raw bits -> bits
        | Mode5Raw bits -> bits

    let value cnf =
        cnf
        |> bits
        |> fun raw -> raw.Value
        |> ConfigurationFieldBitsRaw.value

    let parse : Parser<ParsedField<ConfigurationFieldRaw>> =
        parser {
            let! bits = ConfigurationFieldBitsRaw.parse

            let value =
                bits.Value
                |> ConfigurationFieldBitsRaw.value

            match Mode.tryMap value with
            | Some Mode.Mode0 ->
                return bits |> ParsedField.map (fun _ -> Mode0Raw bits)

            | Some Mode.Mode5 ->
                return bits |> ParsedField.map (fun _ -> Mode5Raw bits)

            | None ->
                return! failBefore 2 $"Encryption mode not supported: 0x{value:X4}"
        }

module BitFields =

    let private maskHop = 0x1us
    let private maskRepeaterAccess = 0x2us
    let private maskSynchronized = 0x20us
    let private maskAccessibility = 0x40us
    let private maskBidirectionalCommunication = 0x80us

    let private map mask cnf =
        cnf &&& mask <> 0us

    let mapHopCounter cnf =
        map maskHop cnf

    let mapRepeaterAccess cnf =
        map maskRepeaterAccess cnf

    let mapSynchronized cnf =
        map maskSynchronized cnf

    let mapAccessibility cnf =
        map maskAccessibility cnf

    let mapBidirectionalCommunication cnf =
        map maskBidirectionalCommunication cnf

module RepeaterAccess =

    let private mask = 0x2us

    let map cnf =
        (cnf &&& mask) <> 0us

module Synchronized =

    let private mask = 0x20us

    let map cnf =
        (cnf &&& mask) <> 0us

module Accessibility =

    let private mask = 0x40us

    let map cnf =
        (cnf &&& mask) <> 0us

type ConfigurationFieldMode0 =
    {
        HopCounter: bool
        RepeaterAccess: bool
        ContentOfMsg: ContentOfMessage
        Mode: Mode
        Synchronized: bool
        Accessibility: bool
        BidirectionalCommunication: bool
    }

module ConfigurationFieldMode0 =

    let fromRaw
        (raw: ParsedField<ConfigurationFieldBitsRaw>)
        : Validation<ConfigurationFieldMode0> =

        validator {
            let cnf = ConfigurationFieldBitsRaw.value raw.Value

            let! cc =
                match ContentOfMessage.tryMap cnf with
                | Some c -> passed c
                | None -> failed raw "Invalid ContentOfMessage"

            return {
                HopCounter = BitFields.mapHopCounter cnf
                RepeaterAccess = BitFields.mapRepeaterAccess cnf
                ContentOfMsg = cc
                Mode = Mode.Mode0
                Synchronized = BitFields.mapSynchronized cnf
                Accessibility = BitFields.mapAccessibility cnf
                BidirectionalCommunication = BitFields.mapBidirectionalCommunication cnf
            }
        }


type ConfigurationFieldMode5 =
    {
        HopCounter: bool
        RepeaterAccess: bool
        ContentOfMsg: ContentOfMessage
        NumberOfEncryptedBlocks: uint8
        Mode: Mode
        Synchronized: bool
        Accessibility: bool
        BidirectionalCommunication: bool
    }

module ConfigurationFieldMode5 =

    let private maskNumberOfEncryptedBlocks = 0xF0us
    let private shiftNumberOfEncryptedBlocks = 4

    let private mapNumberOfEncryptedBlocks cnf =
        (cnf &&& maskNumberOfEncryptedBlocks) >>> shiftNumberOfEncryptedBlocks
        |> byte

    let fromRaw
        (raw: ParsedField<ConfigurationFieldBitsRaw>)
        : Validation<ConfigurationFieldMode5> =

        validator {
            let cnf = ConfigurationFieldBitsRaw.value raw.Value

            let! cc =
                match ContentOfMessage.tryMap cnf with
                | Some c -> passed c
                | None -> failed raw "Invalid ContentOfMessage"

            return {
                HopCounter = BitFields.mapHopCounter cnf
                RepeaterAccess = BitFields.mapRepeaterAccess cnf
                ContentOfMsg = cc
                NumberOfEncryptedBlocks = mapNumberOfEncryptedBlocks cnf
                Mode = Mode.Mode5
                Synchronized = BitFields.mapSynchronized cnf
                Accessibility = BitFields.mapAccessibility cnf
                BidirectionalCommunication = BitFields.mapBidirectionalCommunication cnf
            }
        }