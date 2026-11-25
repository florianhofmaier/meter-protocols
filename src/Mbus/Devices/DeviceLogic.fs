namespace Metering.Mbus.Devices

open System.IO
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus.UserData
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Messages

module ApplicationSelection = Metering.Mbus.Protocol.Frames.ApplicationLayer.ApplicationResetOrSelect

type LinkReadout =
    | Initial
    | Normalized of lastFcb: bool
    | Interrupted of lastFcb: bool option
    | Confirmed of lastFcb: bool * interrupted: bool
    | Reading of lastFcb: bool * blocks: RspUdData list * index: int

type DeviceState = {
    IsSelected: bool
    PrimaryAddress: PrimaryAddress
    SecondaryAddress: MeterAddress
    AccessNumber: AccessNumber
    Status: StatusByte
    PrimaryLink: LinkReadout
    SecondaryLink: LinkReadout
    FirstBlock: int
    SelectedApplicationResponse: bool
}

type DeviceAction =
    | NoAction
    | SendConfirmation
    | SendNegativeConfirmation
    | SendUserData of ResponseUserData
    | SendSelectedApplication of int * ResponseUserData

module DeviceLogic =

    let initialState primaryAddress secondaryAddress = {
        IsSelected = false
        PrimaryAddress = primaryAddress
        SecondaryAddress = secondaryAddress
        AccessNumber = AccessNumber.create 0uy
        Status = {
            ApplicationError = ApplicationError.NoError
            PowerLow = false
            PermanentError = false
            TemporaryError = false
            Bit5 = false
            Bit6 = false
            Bit7 = false
        }
        PrimaryLink = Initial
        SecondaryLink = Initial
        FirstBlock = 0
        SelectedApplicationResponse = false
    }

    let private isAddressed state =
        function
        | AField.Unconfigured -> state.PrimaryAddress = PrimaryAddress.Unconfigured
        | AField.Configured address -> state.PrimaryAddress = PrimaryAddress.PrimAdr address
        | AField.SelectionOfDevice -> state.IsSelected
        | AField.Diagnosis
        | AField.Broadcast -> true
        | _ -> false

    let private link state address =
        if address = AField.SelectionOfDevice then
            state.SecondaryLink
        else
            state.PrimaryLink

    let private withLink state address value =
        if address = AField.SelectionOfDevice then
            { state with SecondaryLink = value }
        else
            { state with PrimaryLink = value }

    let private interrupt =
        function
        | Reading(last, blocks, index) when index < blocks.Length - 1 -> Interrupted(Some last)
        | value -> value

    let private interruptReadout state = {
        state with
            PrimaryLink = interrupt state.PrimaryLink
            SecondaryLink = interrupt state.SecondaryLink
    }

    /// The runtime fetches the published data only when a new sequence is needed.
    let needsUserData state message =
        match message with
        | Message.PrimaryStationMessage(PrimaryStationMessage.RequestUserData request) when
            isAddressed state request.Address && request.Address <> AField.Broadcast
            ->
            match link state request.Address with
            | Initial -> true
            | Normalized last -> request.Fcb <> last
            | Confirmed(last, interrupted) -> request.Fcb <> last && not interrupted
            | Reading(last, blocks, index) -> last <> request.Fcb && index = blocks.Length - 1
            | Interrupted _ -> false
        | Message.PrimaryStationMessage(PrimaryStationMessage.RequestSelectedApplication request) when
            request.Immediate
            && isAddressed state request.Address
            && request.Address <> AField.Broadcast
            ->
            true
        | _ -> false

    let private response state data = {
        Address =
            match state.PrimaryAddress with
            | PrimaryAddress.Unconfigured -> AField.Unconfigured
            | PrimaryAddress.PrimAdr address -> AField.Configured address
        Acd = false
        Dfc = false
        Status = state.Status
        MeterAddress = state.SecondaryAddress
        UserData = RspUdData.records data
        MfrData = RspUdData.mfrData data
        MoreFollows = RspUdData.moreFollows data
    }

    let private request state userData (request: RequestUserData) =
        let start () =
            let blocks =
                match Readout.prepare state.SelectedApplicationResponse userData with
                | Ok blocks -> blocks
                | Error error -> raise (InvalidDataException(error))

            let index =
                if state.FirstBlock < blocks.Length then
                    state.FirstBlock
                else
                    0

            Some(blocks, index)

        let selected =
            match link state request.Address with
            | Initial -> start ()
            | Normalized last when request.Fcb <> last -> start ()
            | Confirmed(last, false) when request.Fcb <> last -> start ()
            | Normalized _
            | Confirmed _
            | Interrupted _ -> None
            | Reading(last, blocks, index) when last = request.Fcb -> Some(blocks, index)
            | Reading(_, blocks, index) when index < blocks.Length - 1 -> Some(blocks, index + 1)
            | Reading _ -> start ()

        match selected with
        | None ->
            match link state request.Address with
            | Confirmed(last, _) when last = request.Fcb -> state, SendConfirmation
            | _ -> state, NoAction
        | Some(blocks, index) ->
            let output = response state blocks[index]
            let state = withLink state request.Address (Reading(request.Fcb, blocks, index))

            let next = {
                state with
                    AccessNumber =
                        AccessNumber.create (
                            byte ((int (AccessNumber.value state.AccessNumber) + 1) &&& 255)
                        )
                    FirstBlock = 0
            }

            next,
            (if state.SelectedApplicationResponse then
                 SendSelectedApplication(index, output)
             else
                 SendUserData output)

    let private restart =
        function
        | Reading(last, _, _)
        | Confirmed(last, _)
        | Normalized last -> Normalized last
        | Interrupted(Some last) -> Normalized last
        | Interrupted None
        | Initial -> Initial

    let handleFrame (state: DeviceState) (userData: RspUdData) (msg: Message) =
        match msg with
        | Message.PrimaryStationMessage message ->
            match message with
            | PrimaryStationMessage.SelectionOfDevice msg ->
                let selected = DeviceSelection.matches msg.Selection state.SecondaryAddress

                {
                    interruptReadout state with
                        IsSelected = selected
                },
                (if selected then SendConfirmation else NoAction)
            | PrimaryStationMessage.LinkLayerReset msg when isAddressed state msg.Address ->
                let current = link state msg.Address

                let normalized =
                    match interrupt current with
                    | Interrupted _ -> Interrupted(Some false)
                    | _ -> Normalized false

                let next = withLink (interruptReadout state) msg.Address normalized

                let next = {
                    next with
                        IsSelected = state.IsSelected && msg.Address <> AField.SelectionOfDevice
                }

                next,
                (if msg.Address = AField.Broadcast then
                     NoAction
                 else
                     SendConfirmation)
            | PrimaryStationMessage.RequestUserData msg when
                isAddressed state msg.Address && msg.Address <> AField.Broadcast
                ->
                request state userData msg
            | PrimaryStationMessage.SendUserData2 msg when isAddressed state msg.Address ->
                interruptReadout state,
                (if msg.Address = AField.Broadcast then
                     NoAction
                 else
                     SendNegativeConfirmation)
            | PrimaryStationMessage.RequestAlarms msg when isAddressed state msg.Address ->
                if msg.Address = AField.Broadcast then
                    state, NoAction
                else
                    match link state msg.Address with
                    | Normalized last when last = msg.Fcb -> state, NoAction
                    | Reading(last, _, _) when last = msg.Fcb ->
                        request state userData { Address = msg.Address; Fcb = msg.Fcb }
                    | Confirmed(last, _) when last = msg.Fcb -> state, SendConfirmation
                    | previous ->
                        let interrupted =
                            match interrupt previous with
                            | Interrupted _ -> true
                            | Confirmed(_, value) -> value
                            | _ -> false

                        withLink
                            (interruptReadout state)
                            msg.Address
                            (Confirmed(msg.Fcb, interrupted)),
                        SendConfirmation
            | PrimaryStationMessage.ChangePrimaryAddress msg when isAddressed state msg.OldAddress ->
                let next = {
                    interruptReadout state with
                        PrimaryAddress = msg.NewAddress
                        PrimaryLink = Initial
                }

                next,
                (if msg.OldAddress = AField.Broadcast then
                     NoAction
                 else
                     SendConfirmation)
            | PrimaryStationMessage.ApplicationResetOrSelect msg when isAddressed state msg.Address ->
                let application, block = ApplicationSelection.selection msg.Selection

                let next = {
                    state with
                        PrimaryLink = restart state.PrimaryLink
                        SecondaryLink = restart state.SecondaryLink
                        FirstBlock = (if application = 0 || application = 1 then block else 0)
                        SelectedApplicationResponse = false
                }

                next,
                (if msg.Address = AField.Broadcast then
                     NoAction
                 else
                     SendConfirmation)
            | PrimaryStationMessage.RequestSelectedApplication msg when
                isAddressed state msg.Address
                ->
                let next = {
                    state with
                        PrimaryLink = restart state.PrimaryLink
                        SecondaryLink = restart state.SecondaryLink
                        SelectedApplicationResponse = true
                }

                if msg.Address = AField.Broadcast then
                    next, NoAction
                elif msg.Immediate then
                    request (withLink next msg.Address Initial) userData {
                        Address = msg.Address
                        Fcb = false
                    }
                else
                    next, SendConfirmation
            | _ -> state, NoAction
        | _ -> state, NoAction
