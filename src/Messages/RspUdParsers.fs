namespace Mbus

open System
open Mbus.BaseParsers.Core
open Mbus.Frames
open Mbus.Records
open Mbus.Records.ValueInfoBlocks
open Mbus.Messages.Converters
open Mbus.Records.DataInfoBlocks

module RspUdParser =

    open System.Collections.Generic

    let getBaseFields (dr: RspDataRecord) =
        let unit = MbusUnit.fromRspRecord dr
        let fn = dr.Fn
        let storageNum = StorageNumber.value dr.StNum
        let tariff = dr.Tariff |> Tariff.value
        let subUnit = dr.SubUnit |> SubUnit.value
        unit, fn, storageNum, tariff, subUnit

    type RecordType =
        | Num of MbusNumericalRecord
        | Txt of MbusTextRecord
        | Mfr of ReadOnlyMemory<byte> * bool
        | Filler

    let vibToTxt vib =
        match vib with
        | RspVib.Text txt -> txt
        | RspVib.Normal normalVib -> normalVib.Def.Val |> MbusValueType.toString
        | RspVib.Mfr mfrVib -> mfrVib.ToArray() |> BitConverter.ToString |> sprintf "Manufacturer specific VIB: %s"
        | RspVib.Invalid inv -> inv.ToArray() |> BitConverter.ToString |> sprintf "Invalid VIB: %s"

    let failIfNotNormalVib vib =
        match vib with
        | RspVib.Normal normalVib -> normalVib
        | _ -> failwith "Expected normal VIB for numerical record"

    let getValueType (vib: NormalRspVib) =
        vib.Def.Val

    let createTxtRec dr =
        let unit, fn, storageNum, tariff, subUnit = getBaseFields dr
        let valueType = vibToTxt dr.Vib
        let value = MbusValue.getTextValue dr
        Txt (MbusTextRecord(unit, fn, storageNum, tariff, subUnit, value, valueType))

    let createNumRec dr =
        let unit, fn, storageNum, tariff, subUnit = getBaseFields dr
        let vib = dr.Vib |> failIfNotNormalVib |> getValueType
        let value = MbusValue.getNumericValue dr
        Num (MbusNumericalRecord(unit, fn, storageNum, tariff, subUnit, value, vib))

    let handleDataRecord (dr: RspDataRecord) =
        match dr.Value, dr.Vib with
        | VarLen _, _ | _, RspVib.Text _ | _, RspVib.Mfr _ | _, RspVib.Invalid _ -> createTxtRec dr
        | _ -> createNumRec dr
        |> Some

    let handleSpecialFunction sf =
        match sf with
        | SpecialFunction.MfrData data ->
            Mfr (data, false) |> Some
        | SpecialFunction.MfrDataMoreFollows data ->
            Mfr (data, true) |> Some
        | _ -> None

    let toRecordType r =
        match r with
        | RspRecord.Data dr -> handleDataRecord dr
        | RspRecord.SpecialFunction sf -> handleSpecialFunction sf

    let getRecords (recList: RspDataRecord list) : IReadOnlyList<MbusNumericalRecord> * IReadOnlyList<MbusTextRecord> * ReadOnlyMemory<byte> * bool =
        let numRecs, txtRecs, mfrData, moreFollows =
            recList
            |> List.map handleDataRecord
            |> List.choose id
            |> List.fold (fun (nums, txts, mfr, more) record ->
                match record with
                | Num n -> (n :: nums, txts, mfr, more)
                | Txt t -> (nums, t :: txts, mfr, more)
                | Mfr (data, moreData) -> (nums, txts, data, moreData)
                | Filler -> (nums, txts, mfr, more)
            ) ([], [], ReadOnlyMemory.Empty, false)

        numRecs |> List.rev :> IReadOnlyList<MbusNumericalRecord>,
        txtRecs |> List.rev :> IReadOnlyList<MbusTextRecord>,
        mfrData,
        moreFollows

    let createMsg frame tpl (apl: RspUdData) =
        let prmAdr = int frame.PrmAdr
        let sndAdr = tpl.Ala
        let status = tpl.Status
        let numData, txtData, mfrData, moreFollows = getRecords apl.DataRecords
        prmAdr, sndAdr, status, numData, txtData, mfrData.ToArray(), moreFollows

    let parse =
        parser {
            let! frame = FrameParser.parseLongFrame
            match frame.Tpl with
            | Tpl.Long tpl ->
                match tpl.Func with
                | TplLongFunc.Rsp ->
                    match frame.Apl with
                    | RspUdData apl -> return createMsg frame tpl apl
                    | _ -> return! fail "Expected APL with data records"
                | _ -> return! fail "Expected Ci field: 0x72"
            | _ -> return! fail "Expected long TPL"
        }
