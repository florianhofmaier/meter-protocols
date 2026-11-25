namespace Metering.Mbus.Codec

open System
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Decoders
open Metering.Common.Decoding.Decoders.Core
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Parsers.Utility
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Frames.WiredMbus
open Metering.Mbus.Protocol.Messages

module CoreDecoder = Metering.Common.Decoding.Decoders.Core.Decoder

/// Routes DLL results to selection or higher-layer decoding, then Message.
module Decoder =

    let private peekCi source =
        CoreDecoder.parseValue
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
                return LinkUserData.MbusProtocol(MbusProtocolUserData.Complete data)
        }

    let private assembleFrame resolver (dll: Field<Dll>) =
        decoder {
            let! frame =
                decoder {
                    match dll.Value with
                    | Dll.SingleCharacter ->
                        return WiredMbusFrame.SingleCharacter(Field.withValue () dll)
                    | Dll.FixedLength frame ->
                        return WiredMbusFrame.FixedLength(Field.withValue frame dll)
                    | Dll.VariableLength frame ->
                        let! linkUserData =
                            decodeLinkUserData
                                resolver
                                (frame.LinkUserData |> Field.map LinkUserDataBytes.bytes)

                        return
                            WiredMbusFrame.VariableLength(Field.withValue frame dll, linkUserData)
                }

            return Field.withValue frame dll
        }

    /// Internal frame boundary for layer-level regression checks, before semantic Message mapping.
    let internal decodeFrame resolver source =
        decoder {
            let! dll = DllCodec.decode source
            return! assembleFrame resolver dll
        }

    let private decodeMessage
        (resolver: IExternalSecurityContextResolver)
        (source: Field<ReadOnlyMemory<byte>>)
        : Decoder<Field<Message>> =

        decoder {
            let! dll = DllCodec.decode source

            let opaqueSend =
                match dll.Value with
                | Dll.VariableLength frame ->
                    let bytes = LinkUserDataBytes.bytes frame.LinkUserData.Value

                    let selectedRequest =
                        not bytes.IsEmpty
                        && (bytes.Span[0] = 0x54uy
                            || bytes.Span[0] = 0x55uy
                            || bytes.Span[0] = 0x56uy)

                    match frame.CField.Value with
                    | CField.Primary control when
                        control.Func.Value = PrimaryFunction.SendUserData
                        && not control.Fcv
                        && not selectedRequest
                        ->
                        Some {
                            SendUserData2.Address = frame.AField.Value
                            Data = frame.LinkUserData.Value
                        }
                    | _ -> None
                | _ -> None

            match opaqueSend with
            | Some command ->
                // EN 13757-2:2024, 5.7.3.4: dispatch the unsupported link service before higher-layer processing.
                return
                    Field.withValue
                        (Message.PrimaryStationMessage(PrimaryStationMessage.SendUserData2 command))
                        dll
            | None ->
                let! frame = assembleFrame resolver dll
                return! Message.decode frame
        }

    /// Runs a bounded input with its own sources and field identities.
    let internal run name decode (bytes: ReadOnlyMemory<byte>) =
        let createReader data offset = ByteReaderFactory.Create(data, offset)
        let store = InMemorySourceStore(createReader) :> ISourceStore
        let root = store.AddRoot name bytes false
        let mutable nextId = 0

        let trace = {
            new IFieldTracer with
                member _.SourceCreated _ = ()

                member _.BeginField(_, _) =
                    let id = FieldId.create nextId
                    nextId <- nextId + 1
                    id

                member _.EndField(_, _) = ()
                member _.FailField(_, _) = ()
        }

        let source = {
            Id = FieldId.create -1
            Span = {
                Source = root.Id
                Offset = 0
                Length = bytes.Length
            }
            Value = bytes
        }

        let context = {
            CreateReader = createReader
            Sources = store
            Trace = trace
        }

        decode source context

    /// Creates a decoder that holds its external security context resolver.
    /// Each invocation creates an independent source store and decoding context.
    /// Input memory must remain stable while returned values reference it.
    let create
        (resolver: IExternalSecurityContextResolver)
        : ReadOnlyMemory<byte> -> DecodeResult<Message> =
        run "M-Bus input" (fun source ->
            decoder {
                let! message = decodeMessage resolver source
                return message.Value
            })
