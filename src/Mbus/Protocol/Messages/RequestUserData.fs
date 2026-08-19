namespace Metering.Mbus.Protocol.Messages

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.WiredMbus


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RequestUserData =

    let private addressFromAField =
        function
        | { Value = AField.Unconfigured } ->
            passed RequestUserDataAddress.Unconfigured

        | { Value = AField.Configured address } ->
            passed (RequestUserDataAddress.Configured address)

        | { Value = AField.SelectionOfDevice } ->
            passed RequestUserDataAddress.SelectionOfDevice

        | { Value = AField.Diagnosis } ->
            passed RequestUserDataAddress.Diagnosis

        | { Value = AField.RepeaterMgmt } as field ->
            failed field "REQ_UD2 does not allow primary master repeater management address 251"

        | { Value = AField.Broadcast } as field ->
            failed field "REQ_UD2 requires a response and does not allow broadcast address 255"

    let private frameFailed frame message =
        let fieldId =
            match frame with
            | WiredMbusFrame.SingleCharacter field ->
                field.Id
            | WiredMbusFrame.FixedLength field ->
                field.Id
            | WiredMbusFrame.VariableLength field ->
                field.Id

        Failed (
            Failures.single {
                FieldId = fieldId
                Message = message
            },
            []
        )

    let matchesFrame =
        function
        | WiredMbusFrame.FixedLength
            {
                Value = {
                    CField = {
                        Value = CField.Primary {
                            Func = { Value = PrimaryFunction.RequestUserDataClass2 }
                        }
                    }
                }
            } ->
            true

        | _ ->
            false

    let fromFrame
        (frame: WiredMbusFrame)
        : Validation<RequestUserData> =

        validator {
            match frame with
            | WiredMbusFrame.FixedLength
                {
                    Value = {
                        CField = ({
                            Value = CField.Primary
                                {
                                    Fcb = fcb
                                    Fcv = fcv
                                    Func = { Value = PrimaryFunction.RequestUserDataClass2 }
                                }
                        } as cField)
                        AField = aField
                    }
                } ->

                let! () =
                    ensure cField "REQ_UD2 requires FCV to be set" fcv

                let! address =
                    addressFromAField aField

                return
                    {
                        Fcb = fcb
                        Address = address
                    }

            | _ ->
                return!
                    frameFailed
                        frame
                        "frame is not a REQ_UD2 request user data message"
        }

    let tryFromFrame
        (frame: WiredMbusFrame)
        : RequestUserData option =

        match fromFrame frame with
        | Passed (message, _) ->
            Some message

        | Failed _ ->
            None
