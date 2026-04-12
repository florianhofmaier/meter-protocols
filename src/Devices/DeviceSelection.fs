module Mbus.Devices.DeviceSelection

open Mbus
open Mbus.Frames
open Mbus.BaseWriters.Core

let private matchBcdNibble deviceNibble selectionNibble =
    selectionNibble = 0xFuy || deviceNibble = selectionNibble

let private matchBcdByte deviceByte selectionByte =
    let highNibble b = (b >>> 4) &&& 0x0Fuy
    let lowNibble b = b &&& 0x0Fuy
    let deviceLow = lowNibble deviceByte
    let deviceHigh = highNibble deviceByte
    let selectionLow = lowNibble selectionByte
    let selectionHigh = highNibble selectionByte
    matchBcdNibble deviceLow selectionLow && matchBcdNibble deviceHigh selectionHigh

let private matchBinaryByte (deviceByte: byte) (selectionByte: byte) =
    selectionByte = 0xFFuy || deviceByte = selectionByte

let private addressToBytes (address: MbusAddress) : byte[] =
    let wState = WState.create()
    match AddressWriter.writeAla address wState with
    | Error err -> failwith $"failed to serialize secondary address: {err}"
    | Ok (_, wState') ->
        let buf = WState.buf wState'
        buf[0..7]

let isSelected (deviceAddress: MbusAddress) (selection: byte[]) : bool =
    let deviceBytes = addressToBytes deviceAddress
    matchBcdByte deviceBytes[0] selection[0] &&
    matchBcdByte deviceBytes[1] selection[1] &&
    matchBcdByte deviceBytes[2] selection[2] &&
    matchBcdByte deviceBytes[3] selection[3] &&
    matchBinaryByte deviceBytes[4] selection[4] &&
    matchBinaryByte deviceBytes[5] selection[5] &&
    matchBinaryByte deviceBytes[6] selection[6] &&
    matchBinaryByte deviceBytes[7] selection[7]
