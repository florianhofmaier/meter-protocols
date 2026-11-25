namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.ErrorHandling
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Utility

type ExtendedSelectionOfDeviceRaw = private RawExtendedSelectionOfDevice of ReadOnlyMemory<uint8>

module ExtendedSelectionOfDeviceRaw =

    let bytes (RawExtendedSelectionOfDevice bytes) = bytes

    let private parseExt: Parser<Field<ExtendedSelectionOfDeviceRaw>> =
        parseField "Extended Selection"
        <| parser {
            do! expectU8 0x0Cuy
            do! expectU8 0x78uy
            return! take 4
        }
        |>> Field.map RawExtendedSelectionOfDevice

    let parse: Parser<Field<ExtendedSelectionOfDeviceRaw> option> =
        parser {
            let! remaining = remaining

            if remaining > 0 then
                return! parseExt |>> Some
            else
                return None
        }

type SecondaryAddressSelectionRaw = {
    Ci: Field<CiField>
    IdNum: Field<IdNumberRaw>
    Mfr: Field<ManufacturerRaw>
    Version: Field<VersionRaw>
    DevType: Field<DeviceTypeRaw>
    Extended: Field<ExtendedSelectionOfDeviceRaw> option
}

type DeviceSelectionRaw =
    | AllDevices of ci: Field<CiField>
    | BySecondaryAddress of SecondaryAddressSelectionRaw

module DeviceSelectionRaw =

    let private isSelectionOfDevice (ciField: CiField) =
        match ciField with
        | LowerLayerManagement CiLowerLayerManagement.SelectionOfDevice -> true

        | _ -> false

    let parse: Parser<Field<DeviceSelectionRaw>> =
        parseField "Link Layer User Data"
        <| parser {
            let! ci = CiField.parse

            if not (isSelectionOfDevice ci.Value) then
                let code = CiField.code ci.Value

                return!
                    failBefore
                        1
                        $"Expect CI-Field for selection of device (0x52), got 0x{code:X2} instead."

            let! count = remaining

            if count = 0 then
                return DeviceSelectionRaw.AllDevices ci
            else
                let! idNum = IdNumberRaw.parse
                let! mfr = ManufacturerRaw.parse
                let! version = VersionRaw.parse
                let! devType = DeviceTypeRaw.parse
                let! extended = ExtendedSelectionOfDeviceRaw.parse

                return
                    DeviceSelectionRaw.BySecondaryAddress {
                        Ci = ci
                        IdNum = idNum
                        Mfr = mfr
                        Version = version
                        DevType = devType
                        Extended = extended
                    }
        }

type IdNumberSelection = private IdNumberSelectionPattern of pattern: uint32 * mask: uint32

type ManufacturerSelection = private ManufacturerSelectionPattern of pattern: uint16 * mask: uint16

type VersionSelection = private VersionSelectionPattern of pattern: uint8 * mask: uint8

type DeviceTypeSelection = private DeviceTypeSelectionPattern of pattern: uint8 * mask: uint8

module IdNumberSelection =

    let private digitCount = 8

    let validate (raw: Field<IdNumberRaw>) : Validation<IdNumberSelection> =

        let value = IdNumberRaw.toUint32 raw.Value

        match Bcd.tryFindInvalidBcdOrWildcardNibble digitCount value with
        | Some(_, nibble) ->
            failed
                raw
                $"Invalid BCD/wildcard nibble 0x{nibble:X} in selection identification number"

        | None ->
            let pattern, mask = Bcd.patternAndMaskUInt32 digitCount value
            passed (IdNumberSelectionPattern(pattern, mask))

    let matches (IdNumberSelectionPattern(pattern, mask)) (idNumber: IdNumber) : bool =

        let value = IdNumber.toBcd idNumber
        (value &&& mask) = pattern

    /// Writes BCD digits and F wildcard nibbles.
    let write (IdNumberSelectionPattern(pattern, mask)) =
        writeU32LittleEndian (pattern ||| ~~~mask)

module private ManufacturerSelectionBytes =

    let private low value = byte value

    let private high value = byte (value >>> 8)

    let patternAndMask value =
        let lowMask = if low value = 0xFFuy then 0x00us else 0x00FFus
        let highMask = if high value = 0xFFuy then 0x0000us else 0xFF00us
        let mask = lowMask ||| highMask

        value &&& mask, mask

module ManufacturerSelection =

    let validate (raw: Field<ManufacturerRaw>) : Validation<ManufacturerSelection> =

        let value = ManufacturerRaw.toUint16 raw.Value
        let pattern, mask = ManufacturerSelectionBytes.patternAndMask value

        passed (ManufacturerSelectionPattern(pattern, mask))

    let matches (ManufacturerSelectionPattern(pattern, mask)) (manufacturer: Manufacturer) : bool =

        let value = Manufacturer.value manufacturer
        (value &&& mask) = pattern

    /// Writes manufacturer bytes and FF wildcards.
    let write (ManufacturerSelectionPattern(pattern, mask)) =
        writeU16LittleEndian (pattern ||| ~~~mask)

module VersionSelection =

    let validate (raw: Field<VersionRaw>) : Validation<VersionSelection> =

        let value = VersionRaw.value raw.Value

        if value = 0xFFuy then
            passed (VersionSelectionPattern(0uy, 0uy))
        else
            passed (VersionSelectionPattern(value, 0xFFuy))

    let matches (VersionSelectionPattern(pattern, mask)) (version: Version) : bool =

        let value = Version.value version
        (value &&& mask) = pattern

    /// Writes the version byte or FF wildcard.
    let write (VersionSelectionPattern(pattern, mask)) = writeU8 (pattern ||| ~~~mask)

module DeviceTypeSelection =

    let validate (raw: Field<DeviceTypeRaw>) : Validation<DeviceTypeSelection> =

        let value = DeviceTypeRaw.value raw.Value

        if value = 0xFFuy then
            passed (DeviceTypeSelectionPattern(0uy, 0uy))
        else
            passed (DeviceTypeSelectionPattern(value, 0xFFuy))

    let matches (DeviceTypeSelectionPattern(pattern, mask)) (deviceType: DeviceType) : bool =

        let value = DeviceType.value deviceType
        (value &&& mask) = pattern

    /// Writes the device-type byte or FF wildcard.
    let write (DeviceTypeSelectionPattern(pattern, mask)) = writeU8 (pattern ||| ~~~mask)

type SecondaryAddressSelection = {
    IdNum: IdNumberSelection
    Mfr: ManufacturerSelection
    Version: VersionSelection
    DevType: DeviceTypeSelection
}

module SecondaryAddressSelection =
    /// Writes the eight secondary-address selection bytes.
    let write (address: SecondaryAddressSelection) =
        writer {
            do! IdNumberSelection.write address.IdNum
            do! ManufacturerSelection.write address.Mfr
            do! VersionSelection.write address.Version
            do! DeviceTypeSelection.write address.DevType
        }

/// CI=52 alone selects all devices; otherwise the secondary address constrains selection.
type DeviceSelection =
    | AllDevices
    | BySecondaryAddress of SecondaryAddressSelection

module DeviceSelection =

    let private validateExtended =
        function
        | Some raw ->
            // EN 13757-7:2018, 8.5, Table 45, p. 52 requires fabrication-number matching.
            failed
                raw
                "Extended device selection is not supported: fabrication-number matching is not implemented"
        | None -> passed ()

    let validate (raw: Field<DeviceSelectionRaw>) : Validation<DeviceSelection> =
        match raw.Value with
        | DeviceSelectionRaw.AllDevices _ -> passed AllDevices
        | DeviceSelectionRaw.BySecondaryAddress address ->
            validator {
                let! idNum = IdNumberSelection.validate address.IdNum
                and! mfr = ManufacturerSelection.validate address.Mfr
                and! version = VersionSelection.validate address.Version
                and! devType = DeviceTypeSelection.validate address.DevType
                and! () = validateExtended address.Extended

                return
                    BySecondaryAddress {
                        IdNum = idNum
                        Mfr = mfr
                        Version = version
                        DevType = devType
                    }
            }

    let matches (selection: DeviceSelection) (device: MeterAddress) : bool =
        match selection with
        | AllDevices -> true
        | BySecondaryAddress address ->
            IdNumberSelection.matches address.IdNum device.IdNum
            && ManufacturerSelection.matches address.Mfr device.Mfr
            && VersionSelection.matches address.Version device.Version
            && DeviceTypeSelection.matches address.DevType device.DevType

    /// Writes CI=52 and the optional secondary address.
    let write selection =
        writer {
            do! writeU8 0x52uy

            match selection with
            | AllDevices -> ()
            | BySecondaryAddress address -> do! SecondaryAddressSelection.write address
        }

module SelectionCodec =

    let parse = DeviceSelectionRaw.parse

    let validate (raw: Field<DeviceSelectionRaw>) : Validation<Field<DeviceSelection>> =
        validateField DeviceSelection.validate raw

    let decode (source: Field<ReadOnlyMemory<byte>>) : Decoder<Field<DeviceSelection>> =
        decoder {
            let! raw = Decoder.parse parse source
            return! Decoder.validate validate raw
        }
