namespace Metering.Mbus.Protocol.Messages

open System
open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.WiredMbus

type ResponseSelectedApplication = {
    Address: AField
    Acd: bool
    Dfc: bool
    Header: (MeterAddress * StatusByte) option
    Data: ReadOnlyMemory<byte>
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResponseSelectedApplication =
    let private response =
        function
        | WiredMbusFrame.VariableLength(dll,
                                        LinkUserData.MbusProtocol(MbusProtocolUserData.Complete message)) ->
            match dll.Value.CField.Value, message.Value.Apl.Value with
            | CField.Secondary control, Apl.SelectedApplicationResponse data ->
                let header =
                    match message.Value.Tpl.Value with
                    | Tpl.LongHeader tpl ->
                        match tpl.Header.Value with
                        | LongHeader.Mode0 h ->
                            Some(DeviceIdentification.toMeterAddress h.Device, h.Status.Value)
                        | LongHeader.Mode5 h ->
                            Some(DeviceIdentification.toMeterAddress h.Device, h.Status.Value)
                    | _ -> None

                Some {
                    Address = dll.Value.AField.Value
                    Acd = control.Acd
                    Dfc = control.Dfc
                    Header = header
                    Data = data
                }
            | _ -> None
        | _ -> None

    let matchesFrame frame = response frame |> Option.isSome

    let fromFrame frame =
        match response frame with
        | Some message -> passed message
        | None ->
            failed (WiredMbusFrame.asField frame) "Expected a response of selected application"

    let write access (message: ResponseSelectedApplication) =
        VariableLengthFrame.write
            (CField.writeSecondary message.Acd message.Dfc)
            message.Address
            (writer {
                match message.Header with
                | None -> do! writeU8 0x66uy
                | Some(identity, status) ->
                    do! writeU8 0x68uy

                    do!
                        LongHeader.writeMode0
                            identity
                            access
                            status
                            ConfigurationFieldMode0.standardData

                do! writeMemory message.Data
            })

    /// The selected standard application carries its subcode followed by the response records.
    let ofUserData index (message: ResponseUserData) =
        match
            Writer.run
                LinkUserDataBytes.maxLength
                (writer {
                    do!
                        writeMemory (
                            ReadOnlyMemory [|
                                byte (index &&& 15)
                                byte ((index >>> 4) &&& 15)
                                byte ((index >>> 8) &&& 15)
                            |]
                        )

                    do! ResponseUserData.writePayload message
                })
        with
        | Error error -> Error error
        | Ok bytes ->
            Ok {
                Address = message.Address
                Acd = message.Acd
                Dfc = message.Dfc
                Header = Some(message.MeterAddress, message.Status)
                Data = ReadOnlyMemory bytes
            }
