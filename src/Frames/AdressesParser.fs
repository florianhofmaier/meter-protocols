module Mbus.Frames.AddressParser

open Mbus
open Mbus.BaseParsers.BcdParsers
open Mbus.BaseParsers.BinaryParsers
open Mbus.BaseParsers.Core

let private parseMfr : Parser<string> =
    parser {
        let! mfrCode = parseU16
        let char1 = char ((mfrCode &&& 0x1Fus) + 64us)
        let char2 = char (((mfrCode >>> 5) &&& 0x1Fus) + 64us)
        let char3 = char (((mfrCode >>> 10) &&& 0x1Fus) + 64us)
        return $"{char3}{char2}{char1}"
    }

let private parseDevType : Parser<MbusDeviceType> =
    parser {
        let! devTypeByte = parseU8
        let devType = enum<MbusDeviceType> (int devTypeByte)
        if System.Enum.IsDefined(typeof<MbusDeviceType>, devType) then
            return devType
        else
            return! failBefore $"invalid device type: 0x{devTypeByte:X2}"
    }

let parseLla : Parser<MbusAddress> =
    parser {
        let! mfr = parseMfr
        let! id = parseBcd8Digit
        let! version = parseU8
        let! devType = parseDevType
        match MbusAddress.create (int id) mfr (int version) devType with
        | Error msg -> return! failBefore msg
        | Ok adr -> return adr
    }

let parseAla : Parser<MbusAddress> =
    parser {
        let! id = parseBcd8Digit
        let! mfr = parseMfr
        let! version = parseU8
        let! devType = parseDevType
        match MbusAddress.create (int id) mfr (int version) devType with
        | Error msg -> return! failBefore msg
        | Ok adr -> return adr
    }