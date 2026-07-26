namespace Metering.Mbus.Protocol.Frames.DataLinkLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.DataLinkLayer.UserData

type LinkUserDataRaw =
    | MbusProtocol of MbusProtocol
    | SelectionOfDevice of SelectionOfDeviceRaw

module LinkUserDataRaw =

    let parse : Parser<Field<LinkUserDataRaw>> =
        parseField "Link User Data"
        <| parser {
            let! ci = CiField.peek

            match ci with
            | LowerLayerManagement
                (CiLowerLayerManagement.SelectionOfDevice _) ->

                return!
                    SelectionOfDeviceRaw.parse
                    |>> LinkUserDataRaw.SelectionOfDevice

            | _ ->
                return!
                    MbusProtocol.parse
                    |>> LinkUserDataRaw.MbusProtocol
        }
