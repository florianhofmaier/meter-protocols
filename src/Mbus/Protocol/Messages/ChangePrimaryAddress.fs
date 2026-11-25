namespace Metering.Mbus.Protocol.Messages

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.WiredMbus
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.DataInfoBlocks
open Metering.Mbus.Protocol.Records.ValueInfoBlocks

type ChangePrimaryAddress = {
    OldAddress: AField
    NewAddress: PrimaryAddress
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ChangePrimaryAddress =

    let private completeSend =
        function
        | WiredMbusFrame.VariableLength(dll,
                                        LinkUserData.MbusProtocol(MbusProtocolUserData.Complete message)) ->
            let tpl = message.Value.Tpl.Value

            let isAddressCommand =
                match tpl with
                | Tpl.NoneHeader tpl -> tpl.Ci.Value = CiFieldTplNoneHeader.Command
                | Tpl.LongHeader tpl -> tpl.Ci.Value = CiFieldTplLongHeader.Command
                | _ -> false

            match dll.Value.CField.Value, message.Value.Apl.Value with
            | CField.Primary control, Apl.SndUdData data when
                control.Func.Value = PrimaryFunction.SendUserData && isAddressCommand
                ->
                Some(dll.Value, control, tpl, SndUdData.records data)
            | _ -> None
        | _ -> None

    let matchesFrame frame = completeSend frame |> Option.isSome

    let private addressValue (record: DataRecordCmd) =
        let dib = record.Dib
        let (VibCmd.Normal vib) = record.Vib

        match record.Value with
        | Value.Int8 value when
            dib.Fn = FunctionField.Inst
            && dib.StNum = StorageNumber.zero
            && dib.Tariff = Tariff.zero
            && dib.SubUnit = SubUnit.zero
            && vib.Def = {
                             Val = MbusValueType.Address
                             Unit = MbusUnit.NoUnit
                             Scaler = 1m
                         }
            && vib.Ext.IsEmpty
            && (vib.Actions.IsEmpty || vib.Actions = [ ActionCode.Set ])
            ->
            // EN 13757-3:2025, Table 10 and A.3: Address overrides DIF=01 with unsigned type C.
            let address = byte (int value &&& 255)

            if address = 0uy then
                Some PrimaryAddress.Unconfigured
            else
                PrimAdr.tryCreate address |> Option.map PrimaryAddress.PrimAdr
        | _ -> None

    /// Maps a single address SET record from CI 51 or long-header CI 5B.
    let validate frame : Validation<ChangePrimaryAddress> =
        validator {
            match completeSend frame with
            | Some(dll, control, tpl, [ record ]) ->
                let! () = ensure dll.CField "SND-UD requires FCV=1." control.Fcv

                let! () =
                    match tpl with
                    | Tpl.LongHeader tpl ->
                        let status =
                            match tpl.Header.Value with
                            | LongHeader.Mode0 header -> header.Status
                            | LongHeader.Mode5 header -> header.Status

                        let value = status.Value
                        // EN 13757-7:2018, 7.5.7, Tables 16/17: wired signal-level value is zero.
                        ensure
                            status
                            "Wired partner status requires zero signal-level value and reserved bit 7 cleared."
                            (value.ApplicationError = ApplicationError.NoError
                             && not value.PowerLow
                             && not value.PermanentError
                             && not value.TemporaryError
                             && not value.Bit5
                             && not value.Bit7)
                    | _ -> passed ()

                match addressValue record with
                | Some address ->
                    return {
                        OldAddress = dll.AField.Value
                        NewAddress = address
                    }
                | None ->
                    return!
                        failed
                            (WiredMbusFrame.asField frame)
                            "Expected one address SET record with an unsigned primary address in 0..250."
            | _ ->
                return!
                    failed
                        (WiredMbusFrame.asField frame)
                        "Frame is not a single SND-UD address SET command."
        }

    /// Writes the address SET record (EN 13757-3:2025, Tables 4/10 and 6.4.7).
    let writePayload (message: ChangePrimaryAddress) : Writer<unit> =
        writer {
            do!
                Dib.write 0x01uy {
                    Fn = FunctionField.Inst
                    StNum = StorageNumber.zero
                    Tariff = Tariff.zero
                    SubUnit = SubUnit.zero
                }

            do!
                Vif.write {
                    Val = MbusValueType.Address
                    Unit = MbusUnit.NoUnit
                    Scaler = 1m
                }

            do! PrimaryAddress.write message.NewAddress
        }

    /// Composes SND-UD with CI 5B and the encoder's resolved long header and payload.
    let writeWithHeader header payload (message: ChangePrimaryAddress) =
        VariableLengthFrame.write
            (CField.writePrimary PrimaryFunction.SendUserData false true)
            message.OldAddress
            (TplWithLongHeader.writeWithHeader
                CiFieldTplLongHeader.Command
                header
                (writeMemory payload))

    /// Writes the plain SND-UD form with CI 51 and the address SET record.
    let write (message: ChangePrimaryAddress) : Writer<unit> =
        VariableLengthFrame.write
            (CField.writePrimary PrimaryFunction.SendUserData false true)
            message.OldAddress
            (writer {
                do! CiFieldTpl.write (CiFieldTpl.NoneHeader CiFieldTplNoneHeader.Command)
                do! writePayload message
            })
