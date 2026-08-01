namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type LinkUserDataRaw =
    | MbusProtocol of MbusProtocolRaw
    | SelectionOfDevice of SelectionOfDeviceRaw

module LinkUserDataRaw =

    let parse
        (securityContextResolver: IExternalSecurityContextResolver)
        : Parser<Field<LinkUserDataRaw>> =
        parseField "Link User Data"
        <| parser {
            let! ci = CiField.peek

            match ci with
            | LowerLayerManagement
                CiLowerLayerManagement.SelectionOfDevice ->

                return!
                    SelectionOfDeviceRaw.parse
                    |>> LinkUserDataRaw.SelectionOfDevice

            | _ ->
                return!
                    MbusProtocolRaw.parse securityContextResolver
                    |>> LinkUserDataRaw.MbusProtocol
        }

