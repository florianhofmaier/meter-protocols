namespace Metering.Mbus.Protocol

open System
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Utility
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Frames.WiredMbus
open Metering.Mbus.Protocol.Messages

/// Routes DLL results to selection or higher-layer decoding, then Message.
module MbusDecoder =

    let private peekCi source =
        Core.parseValue
            (parser {
                let! ci = CiField.peek
                // Consume the opaque view to satisfy the exact decoder boundary.
                let! _ = takeAll
                return ci
            })
            source

    let private decodeLinkUserData resolver source =
        decoder {
            let! ci = peekCi source
            match ci with
            | CiField.LowerLayerManagement CiLowerLayerManagement.SelectionOfDevice ->
                let! selection = SelectionCodec.decode source
                return LinkUserData.SelectionOfDevice selection.Value
            | _ ->
                let! higher = HigherLayersDecoder.decode source
                let! plain = MbusProtection.decode resolver higher.Value.Protection
                let! apl = AplCodec.decode (Tpl.ci higher.Value.Tpl.Value).Value plain
                let complete: CompleteLinkUserData = {
                    Ell = higher.Value.Ell
                    Nwl = higher.Value.Nwl
                    Afl = higher.Value.Afl
                    Tpl = higher.Value.Tpl
                    Apl = apl
                }
                let data = Field.withValue complete higher
                return LinkUserData.MbusProtocol (MbusProtocolUserData.Complete data)
        }

    let decodeFrame
        (resolver: IExternalSecurityContextResolver)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<WiredMbusFrame>> =

        decoder {
            let! dll = DllCodec.decode source
            let! frame =
                decoder {
                    match dll.Value with
                    | Dll.SingleCharacter ->
                        return WiredMbusFrame.SingleCharacter (Field.withValue () dll)
                    | Dll.FixedLength frame ->
                        return WiredMbusFrame.FixedLength (Field.withValue frame dll)
                    | Dll.VariableLength frame ->
                        let! linkUserData = decodeLinkUserData resolver frame.LinkUserData
                        return WiredMbusFrame.VariableLength (Field.withValue frame dll, linkUserData)
                }
            return Field.withValue frame dll
        }

    let decode
        (resolver: IExternalSecurityContextResolver)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<Message>> =

        decoder {
            let! frame = decodeFrame resolver source
            return! Message.decode frame
        }
