namespace Mbus.Devices

open Mbus
open Mbus.Frames
open Mbus.Records

type DeviceState = {
    IsSelected: bool
    PrimaryAddress: byte
    SecondaryAddress: MbusAddress
    AccessNumber: byte
    Status: MbusStatusField
    Cnf: uint16
}

type DeviceAction =
    | NoAction
    | SendFrame of Frame

module DeviceLogic =

    open Mbus.Messages

    let initialState prmAdr secAdr = {
        IsSelected = false
        PrimaryAddress = prmAdr
        SecondaryAddress = secAdr
        AccessNumber = 0uy
        Status = MbusStatusField.CreateEmpty
        Cnf = 0us
    }

    let private createRspUd state userData =
        let addRecords builder =
            userData |> List.fold (fun b r -> RspUdBuilder.addDataRecord r b) builder

        RspUdBuilder.init state.SecondaryAddress
        |> RspUdBuilder.withPrimaryAddress state.PrimaryAddress
        |> RspUdBuilder.withAccessNumber state.AccessNumber
        |> RspUdBuilder.withStatus state.Status
        |> RspUdBuilder.withConfigField state.Cnf
        |> addRecords
        |> RspUdBuilder.build

    let private isPrimAdrMatch state adr =
        if state.PrimaryAddress = adr then true
        elif state.IsSelected && adr = 0xFDuy then true
        else false

    // let private (|SndNke|_|) primaryAddress frame =
    //     match frame with
    //     | Frame.ShortFrame sf when sf.CField = 0x40uy && sf.PrmAdr = primaryAddress ->
    //         Some ()
    //     | _ -> None

    let private (|SelectDevice|_|) frame =
        match frame with
        | Frame.LongFrame { Tpl = CiOnly DevSelect; Apl = SelectedDevice selection } -> Some selection
        | _ -> None

    let private handleSelectDevice state selection =
        if DeviceSelection.isSelected state.SecondaryAddress selection then
            { state with IsSelected = true }, SendFrame Confirmation
        else
            state, NoAction

    let private (|RequestUserData|_|) frame =
        match frame with
        | Frame.ShortFrame sf when CField.isReqUd2 sf.CField ->
            Some sf
        | _ -> None

    let private handleReqUd2 state userData prmAdr=
        if isPrimAdrMatch state prmAdr then
            let response = createRspUd state userData
            state, SendFrame response
        else
            state, NoAction

    let handleFrame (state: DeviceState) (userData: RspDataRecord list) (frame: Frame) : DeviceState * DeviceAction =
        match frame with
        | SelectDevice selection -> handleSelectDevice state selection
        | RequestUserData reqUd2 -> handleReqUd2 state userData reqUd2.PrmAdr

        | _ ->
            state, NoAction
