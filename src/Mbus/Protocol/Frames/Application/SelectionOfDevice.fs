namespace Metering.Mbus.Protocol.Frames.Application

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Binary
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Utility

type ExtendedSelectionOfDeviceRaw =
    private | RawExtendedSelectionOfDevice of ReadOnlyMemory<uint8>

module ExtendedSelectionOfDeviceRaw =

    let bytes (RawExtendedSelectionOfDevice bytes) =
        bytes

    let private parseExt : Parser<Field<ExtendedSelectionOfDeviceRaw>> =
        parseField "Extended Selection"
        <| parser {
            do! expectU8 0x0Cuy
            do! expectU8 0x78uy
            return! take 4
        }
        |>> Field.map RawExtendedSelectionOfDevice

    let parse : Parser<Field<ExtendedSelectionOfDeviceRaw> option> =
        parser {
            let! remaining = remaining
            if remaining > 0 then
                return! parseExt |>> Some
            else
                return None
        }

type ExtendedSelectionOfDevice =
    private | ExtendedSelectionOfDevice of ReadOnlyMemory<uint8>

module ExtendedSelectionOfDevice =

    let bytes (ExtendedSelectionOfDevice bytes) =
        bytes

    let fromRaw
        (raw: Field<ExtendedSelectionOfDeviceRaw>)
        : Validation<ExtendedSelectionOfDevice> =

        let bytes = ExtendedSelectionOfDeviceRaw.bytes raw.Value
        passed (ExtendedSelectionOfDevice bytes)

type SelectionOfDeviceRaw =
    {
        IdNum: Field<IdNumberRaw>
        Mfr: Field<ManufacturerRaw>
        Version: Field<VersionRaw>
        DevType: Field<DeviceTypeRaw>
        Extended: Field<ExtendedSelectionOfDeviceRaw> option
    }

module SelectionOfDeviceRaw =

    let parse : Parser<Field<SelectionOfDeviceRaw>> =
        parseField "Selection of Device"
        <| parser {
            let! idNum = IdNumberRaw.parse
            let! mfr = ManufacturerRaw.parse
            let! version = VersionRaw.parse
            let! devType = DeviceTypeRaw.parse
            let! extended = ExtendedSelectionOfDeviceRaw.parse

            return
                {
                    IdNum = idNum
                    Mfr = mfr
                    Version = version
                    DevType = devType
                    Extended = extended
                }
        }

type IdNumberSelection =
    private
        IdNumberSelectionPattern of pattern: uint32 * mask: uint32

type ManufacturerSelection =
    private
        ManufacturerSelectionPattern of pattern: uint16 * mask: uint16

type VersionSelection =
    private
        VersionSelectionPattern of pattern: uint8 * mask: uint8

type DeviceTypeSelection =
    private
        DeviceTypeSelectionPattern of pattern: uint8 * mask: uint8

module IdNumberSelection =

    let private digitCount = 8

    let fromRaw
        (raw: Field<IdNumberRaw>)
        : Validation<IdNumberSelection> =

        let value = IdNumberRaw.value raw.Value

        match Bcd.tryFindInvalidBcdOrWildcardNibble digitCount value with
        | Some (_, nibble) ->
            failed raw $"Invalid BCD/wildcard nibble 0x{nibble:X} in selection identification number"

        | None ->
            let pattern, mask = Bcd.patternAndMaskUInt32 digitCount value
            passed (IdNumberSelectionPattern (pattern, mask))

    let matches
        (IdNumberSelectionPattern (pattern, mask))
        (idNumber: IdNumber)
        : bool =

        let value = IdNumber.toBcd idNumber
        (value &&& mask) = pattern

module private ManufacturerSelectionBytes =

    let private low value =
        byte value

    let private high value =
        byte (value >>> 8)

    let patternAndMask value =
        let lowMask = if low value = 0xFFuy then 0x00us else 0x00FFus
        let highMask = if high value = 0xFFuy then 0x0000us else 0xFF00us
        let mask = lowMask ||| highMask

        value &&& mask, mask

module ManufacturerSelection =

    let fromRaw
        (raw: Field<ManufacturerRaw>)
        : Validation<ManufacturerSelection> =

        let value = ManufacturerRaw.value raw.Value
        let pattern, mask = ManufacturerSelectionBytes.patternAndMask value

        passed (ManufacturerSelectionPattern (pattern, mask))

    let matches
        (ManufacturerSelectionPattern (pattern, mask))
        (manufacturer: Manufacturer)
        : bool =

        let value = Manufacturer.value manufacturer
        (value &&& mask) = pattern

module VersionSelection =

    let fromRaw
        (raw: Field<VersionRaw>)
        : Validation<VersionSelection> =

        let value = VersionRaw.value raw.Value

        if value = 0xFFuy then
            passed (VersionSelectionPattern (0uy, 0uy))
        else
            passed (VersionSelectionPattern (value, 0xFFuy))

    let matches
        (VersionSelectionPattern (pattern, mask))
        (version: Version)
        : bool =

        let value = Version.value version
        (value &&& mask) = pattern

module DeviceTypeSelection =

    let fromRaw
        (raw: Field<DeviceTypeRaw>)
        : Validation<DeviceTypeSelection> =

        let value = DeviceTypeRaw.value raw.Value

        if value = 0xFFuy then
            passed (DeviceTypeSelectionPattern (0uy, 0uy))
        else
            passed (DeviceTypeSelectionPattern (value, 0xFFuy))

    let matches
        (DeviceTypeSelectionPattern (pattern, mask))
        (deviceType: DeviceType)
        : bool =

        let value = DeviceType.value deviceType
        (value &&& mask) = pattern

type SelectionOfDevice =
    {
        IdNum: IdNumberSelection
        Mfr: ManufacturerSelection
        Version: VersionSelection
        DevType: DeviceTypeSelection
        Extended: ExtendedSelectionOfDevice option
    }

module SelectionOfDevice =

    let private validateExtended
        (raw: Field<ExtendedSelectionOfDeviceRaw> option)
        : Validation<ExtendedSelectionOfDevice option> =

        match raw with
        | Some raw ->
            raw
            |> ExtendedSelectionOfDevice.fromRaw
            |> map Some

        | None ->
            passed None

    let fromRaw
        (raw: Field<SelectionOfDeviceRaw>)
        : Validation<SelectionOfDevice> =

        validator {
            let! idNum = IdNumberSelection.fromRaw raw.Value.IdNum
            and! mfr = ManufacturerSelection.fromRaw raw.Value.Mfr
            and! version = VersionSelection.fromRaw raw.Value.Version
            and! devType = DeviceTypeSelection.fromRaw raw.Value.DevType
            and! extended = validateExtended raw.Value.Extended

            return
                {
                    IdNum = idNum
                    Mfr = mfr
                    Version = version
                    DevType = devType
                    Extended = extended
                }
        }

    let matches
        (selection: SelectionOfDevice)
        (device: DeviceIdentification)
        : bool =

        IdNumberSelection.matches selection.IdNum device.IdNum.Value
        && ManufacturerSelection.matches selection.Mfr device.Mfr.Value
        && VersionSelection.matches selection.Version device.Version.Value
        && DeviceTypeSelection.matches selection.DevType device.DevType.Value
