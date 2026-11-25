namespace Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.DataLinkLayer

/// Wired link-service identity; independent of application/message support and frame integrity.
[<RequireQualifiedAccess>]
type TelegramType =
    | Ack
    | SndNke
    | ReqUd1
    | ReqUd2
    | SndUd
    | SndUd2
    | RspUd

module TelegramType =

    /// Identifies the wired service from frame form and validated C-field semantics.
    /// EN 13757-2+A1:2024, 5.7.3.1–4 and 5.7.7, pp.19–21;
    /// IEC 60870-5-2, 3.2, p.14: single-character acknowledgement.
    /// A result does not assert checksum, payload or complete-message validity.
    let tryIdentify (raw: Field<DllRaw>) : Field<TelegramType> option =
        let identify longFrame control =
            let identity =
                match CField.validate control with
                | Passed(CField.Primary primary, _) ->
                    match longFrame, primary.Func.Value with
                    | false, PrimaryFunction.LinkReset when not primary.Fcv && not primary.Fcb ->
                        Some TelegramType.SndNke
                    | false, PrimaryFunction.RequestUserDataClass1 when primary.Fcv ->
                        Some TelegramType.ReqUd1
                    | false, PrimaryFunction.RequestUserDataClass2 when primary.Fcv ->
                        Some TelegramType.ReqUd2
                    | true, PrimaryFunction.SendUserData ->
                        // 5.7.7: FCV distinguishes the services; delivery is independent of FCB.
                        Some(
                            if primary.Fcv then
                                TelegramType.SndUd
                            else
                                TelegramType.SndUd2
                        )
                    | _ -> None
                | Passed(CField.Secondary secondary, _) when longFrame ->
                    match secondary.Func.Value with
                    | SecondaryFunction.ResponseUserData -> Some TelegramType.RspUd
                | _ -> None

            identity |> Option.map (fun value -> Field.withValue value control)

        match raw.Value with
        | DllRaw.SingleCharacter -> Some(Field.withValue TelegramType.Ack raw)
        | DllRaw.FixedLength frame -> identify false frame.UserData.Value.CField
        | DllRaw.VariableLength frame -> identify true frame.UserData.Value.CField
