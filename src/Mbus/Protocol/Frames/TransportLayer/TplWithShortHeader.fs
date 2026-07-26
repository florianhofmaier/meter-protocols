namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer

type AplDataShortHeaderRaw =
    | ApplicationResetOrSelect of ApplicationResetOrSelectRaw
    | SndUdData of RecordsRaw

type TplWithShortHeaderRaw =
    {
        Ci: Field<Unit>
        Header: Field<ShortHeaderRaw>
        AplData: Field<AplDataShortHeaderRaw>
    }

module TplWithShortHeaderRaw =

    let parse ci : Parser<TplWithShortHeaderRaw> =
        parser {
            let! header = ShortHeaderRaw.parse

            match ci.Value with
            | CiFieldTplShortHeader.Response ->
                let! aplData =
                let! aplData =
                    RecordsRaw.parse
                    |>> Field.map AplDataShortHeaderRaw.SndUdData

                return {
                        Ci = Field.withValue ci ()
                        Header = header
                        AplData = aplData
                    }
        }

type AplDataShortHeader =
    | ApplicationResetOrSelect of ApplicationResetOrSelect
    | SndUdData of SndUdData

type TplWithShortHeader =
    {
        Ci: Field<Unit>
        Header: Field<ShortHeader>
        AplData: Field<AplDataShortHeader>
    }

module TplWithShortHeader =

    let fromRaw
        (raw: TplWithShortHeaderRaw)
        : Validation<TplWithShortHeader> =

        validator {
            let! header = ShortHeader.fromRaw raw.Header

            match raw.AplData.Value with
            | AplDataShortHeaderRaw.ApplicationResetOrSelect rawAplData->
                let! aplData =
                    rawAplData
                    |> Field.withValue raw.AplData
                    |> ApplicationResetOrSelect.fromRaw
                    |> map (Field.map AplDataShortHeader.ApplicationResetOrSelect)

                return {
                    Ci = raw.Ci
                    Header = header
                    AplData = aplData
                }

            | AplDataShortHeaderRaw.SndUdData rawAplData ->
                let! aplData =
                    rawAplData
                    |> SndUdData.fromRaw
                    |> map AplDataShortHeader.SndUdData
                    |> map (Field.withValue raw.AplData)

                return {
                    Ci = raw.Ci
                    Header = header
                    AplData = aplData
                }
        }
