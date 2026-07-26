namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer

type AplDataNoHeaderRaw =
    | SndUdData of RecordsRaw
    | ApplicationResetOrSelect of ApplicationResetOrSelectRaw

type TplWithNoneHeaderRaw =
    {
        Ci: Field<Unit>
        AplData: Field<AplDataNoHeaderRaw>
    }

module TplWithNoneHeaderRaw =

    let parse
        (ci: Field<CiFieldTplNoneHeader>)
        : Parser<TplWithNoneHeaderRaw> =

        parser {
             match ci.Value with
             | CiFieldTplNoneHeader.ApplicationResetOrSelect ->
                 let! aplData =
                     ApplicationResetOrSelectRaw.parse
                     |>> Field.map AplDataNoHeaderRaw.ApplicationResetOrSelect

                 return {
                     Ci = Field.withValue ci ()
                     AplData = aplData
                 }

             | CiFieldTplNoneHeader.Command ->
                 let! aplData =
                     RecordsRaw.parse
                     |>> Field.map AplDataNoHeaderRaw.SndUdData

                 return
                     {
                         Ci = Field.withValue ci ()
                         AplData = aplData
                     }
        }

type AplDataNoHeader =
    | SndUdData of SndUdData
    | ApplicationResetOrSelect of ApplicationResetOrSelect

type TplWithNoneHeader =
    {
        Ci: Field<Unit>
        AplData: Field<AplDataNoHeader>
    }

module TplWithNoneHeader =

    let fromRaw
        (raw: TplWithNoneHeaderRaw)
        : Validation<TplWithNoneHeader> =

        validator {
            match raw.AplData.Value with
            | AplDataNoHeaderRaw.ApplicationResetOrSelect rawAplData ->
                let! aplData =
                    rawAplData
                    |> Field.withValue raw.AplData
                    |> ApplicationResetOrSelect.fromRaw
                    |> map (Field.map AplDataNoHeader.ApplicationResetOrSelect)

                return {
                    Ci = raw.Ci
                    AplData = aplData
                }

            | AplDataNoHeaderRaw.SndUdData rawAplData ->
                let! aplData =
                    rawAplData
                    |> SndUdData.fromRaw
                    |> map AplDataNoHeader.SndUdData
                    |> map (Field.withValue raw.AplData)

                return {
                    Ci = raw.Ci
                    AplData = aplData
                }
        }
