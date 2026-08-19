namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.TransportLayer.Security

type LinkUserDataRaw =
    | MbusProtocol of MbusProtocolUserDataRaw
    | SelectionOfDevice of Field<SelectionOfDeviceRaw>

module LinkUserDataRaw =

    let parse
        (securityContextResolver: IExternalSecurityContextResolver)
        : Parser<LinkUserDataRaw> =
        parser {
            let! ci = CiField.peek

            match ci with
            | LowerLayerManagement
                CiLowerLayerManagement.SelectionOfDevice ->

                return!
                    SelectionOfDeviceRaw.parse
                    |>> LinkUserDataRaw.SelectionOfDevice

            | _ ->
                return!
                    MbusProtocolUserDataRaw.parse securityContextResolver
                    |>> LinkUserDataRaw.MbusProtocol
        }

type LinkUserData =
    | MbusProtocol of MbusProtocolUserData
    | SelectionOfDevice of SelectionOfDevice

module LinkUserData =

    let fromRaw
        (raw: LinkUserDataRaw)
        : Validation<LinkUserData> =

        validator {
            match raw with
            | LinkUserDataRaw.MbusProtocol mbusProtocol ->
                return!
                    MbusProtocolUserData.fromRaw mbusProtocol
                    |> map LinkUserData.MbusProtocol

            | LinkUserDataRaw.SelectionOfDevice selection ->
                return!
                    SelectionOfDevice.fromRaw selection
                    |> map LinkUserData.SelectionOfDevice
        }
