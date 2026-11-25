namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Validators.Core

type ConfigurationFieldBitsRaw = private ConfigurationFieldBits of uint16

type ConfigurationFieldRaw =
    | Mode0Raw of Field<ConfigurationFieldBitsRaw>
    | Mode5Raw of Field<ConfigurationFieldBitsRaw>

module ConfigurationFieldBitsRaw =

    let value (ConfigurationFieldBits v) = v

    let parse: Parser<Field<ConfigurationFieldBitsRaw>> =
        parseField "Configuration Field" parseU16LittleEndian
        |>> Field.map ConfigurationFieldBits

module ConfigurationFieldRaw =

    let bits =
        function
        | Mode0Raw bits -> bits
        | Mode5Raw bits -> bits

    let value cnf =
        cnf |> bits |> _.Value |> ConfigurationFieldBitsRaw.value

    let parse: Parser<Field<ConfigurationFieldRaw>> =
        parser {
            let! bits = ConfigurationFieldBitsRaw.parse

            let value = bits.Value |> ConfigurationFieldBitsRaw.value

            match Mode.tryMap value with
            | Some Mode.Mode0 -> return bits |> Field.map (fun _ -> Mode0Raw bits)

            | Some Mode.Mode5 -> return bits |> Field.map (fun _ -> Mode5Raw bits)

            | None ->
                let mode = Mode.rawValue value |> byte
                return! failBefore 2 $"Unsupported security mode {mode}."
        }

/// H subfield: one-bit hop counter; EN 13757-7:2018, 7.7.1.
[<RequireQualifiedAccess>]
type HopCounter =
    | Zero
    | One

module HopCounter =
    let ofConfigurationBits bits =
        if bits &&& 0x0001us = 0us then
            HopCounter.Zero
        else
            HopCounter.One

    let toConfigurationBits =
        function
        | HopCounter.Zero -> 0us
        | HopCounter.One -> 0x0001us

/// R subfield; reserved for message repetition, cleared on wired M-Bus.
[<RequireQualifiedAccess>]
type RepeaterAccess =
    | NotSet
    | Set

module RepeaterAccess =
    let ofConfigurationBits bits =
        if bits &&& 0x0002us = 0us then
            RepeaterAccess.NotSet
        else
            RepeaterAccess.Set

    let toConfigurationBits =
        function
        | RepeaterAccess.NotSet -> 0us
        | RepeaterAccess.Set -> 0x0002us

/// S subfield: declaration of a synchronized transmission.
[<RequireQualifiedAccess>]
type Synchronization =
    | NotSynchronized
    | Synchronized

module Synchronization =
    let ofConfigurationBits bits =
        if bits &&& 0x2000us = 0us then
            Synchronization.NotSynchronized
        else
            Synchronization.Synchronized

    let toConfigurationBits =
        function
        | Synchronization.NotSynchronized -> 0us
        | Synchronization.Synchronized -> 0x2000us

/// Joint B/A subfields: meter accessibility according to EN 13757-7:2018, Table 23.
[<RequireQualifiedAccess>]
type Accessibility =
    | NoAccessWindow
    | BidirectionalAccessWithoutWindow
    | ShortAccessWindow
    | UnlimitedAccess

module Accessibility =
    let ofConfigurationBits bits =
        match bits &&& 0xC000us with
        | 0x0000us -> Accessibility.NoAccessWindow
        | 0x4000us -> Accessibility.BidirectionalAccessWithoutWindow
        | 0x8000us -> Accessibility.ShortAccessWindow
        | _ -> Accessibility.UnlimitedAccess

    let toConfigurationBits =
        function
        | Accessibility.NoAccessWindow -> 0x0000us
        | Accessibility.BidirectionalAccessWithoutWindow -> 0x4000us
        | Accessibility.ShortAccessWindow -> 0x8000us
        | Accessibility.UnlimitedAccess -> 0xC000us

type ConfigurationFieldMode0 = {
    HopCounter: HopCounter
    RepeaterAccess: RepeaterAccess
    ContentOfMsg: ContentOfMessage
    Synchronized: Synchronization
    Accessibility: Accessibility
}

module ConfigurationFieldMode0 =

    let standardData: ConfigurationFieldMode0 = {
        HopCounter = HopCounter.Zero
        RepeaterAccess = RepeaterAccess.NotSet
        ContentOfMsg = ContentOfMessage.StandardData
        Synchronized = Synchronization.NotSynchronized
        Accessibility = Accessibility.NoAccessWindow
    }

    let validate (raw: Field<ConfigurationFieldBitsRaw>) : Validation<ConfigurationFieldMode0> =

        validator {
            let cnf = ConfigurationFieldBitsRaw.value raw.Value

            let! () =
                ensure
                    raw
                    "Expected configuration security mode 0."
                    (Mode.tryMap cnf = Some Mode.Mode0)

            let! cc =
                match ContentOfMessage.tryMap cnf with
                | Some c -> passed c
                | None -> failed raw "Invalid ContentOfMessage"

            return {
                HopCounter = HopCounter.ofConfigurationBits cnf
                RepeaterAccess = RepeaterAccess.ofConfigurationBits cnf
                ContentOfMsg = cc
                Synchronized = Synchronization.ofConfigurationBits cnf
                Accessibility = Accessibility.ofConfigurationBits cnf
            }
        }

    let write (cnf: ConfigurationFieldMode0) : Writer<unit> =
        let content =
            match cnf.ContentOfMsg with
            | StandardData -> 0us
            | StaticMessage -> 8us

        let bits =
            (0us <<< 8)
            ||| content
            ||| HopCounter.toConfigurationBits cnf.HopCounter
            ||| RepeaterAccess.toConfigurationBits cnf.RepeaterAccess
            ||| Synchronization.toConfigurationBits cnf.Synchronized
            ||| Accessibility.toConfigurationBits cnf.Accessibility

        writeU16LittleEndian bits

type ConfigurationFieldMode5 = {
    HopCounter: HopCounter
    RepeaterAccess: RepeaterAccess
    ContentOfMsg: ContentOfMessage
    NumOfEncryptedBlocks: NumberOfEncryptedBlocks
    Synchronized: Synchronization
    Accessibility: Accessibility
}

module ConfigurationFieldMode5 =

    let validate
        (raw: Field<ConfigurationFieldBitsRaw>)
        : Validation<Field<ConfigurationFieldMode5>> =

        validator {
            let cnf = ConfigurationFieldBitsRaw.value raw.Value

            let! () =
                ensure
                    raw
                    "Expected configuration security mode 5."
                    (Mode.tryMap cnf = Some Mode.Mode5)

            let! cc =
                match ContentOfMessage.tryMap cnf with
                | Some c -> passed c
                | None -> failed raw "Invalid ContentOfMessage"

            let encryptedLength = NumberOfEncryptedBlocks.map cnf

            return
                raw
                |> Field.withValue {
                    HopCounter = HopCounter.ofConfigurationBits cnf
                    RepeaterAccess = RepeaterAccess.ofConfigurationBits cnf
                    ContentOfMsg = cc
                    NumOfEncryptedBlocks = encryptedLength
                    Synchronized = Synchronization.ofConfigurationBits cnf
                    Accessibility = Accessibility.ofConfigurationBits cnf
                }
        }

    let write (cnf: ConfigurationFieldMode5) : Writer<unit> =
        let content =
            match cnf.ContentOfMsg with
            | StandardData -> 0us
            | StaticMessage -> 8us

        let bits =
            (5us <<< 8)
            ||| content
            ||| HopCounter.toConfigurationBits cnf.HopCounter
            ||| RepeaterAccess.toConfigurationBits cnf.RepeaterAccess
            ||| Synchronization.toConfigurationBits cnf.Synchronized
            ||| Accessibility.toConfigurationBits cnf.Accessibility

        let blocks = NumberOfEncryptedBlocks.value cnf.NumOfEncryptedBlocks
        writeU16LittleEndian (bits ||| (uint16 blocks <<< 4))
