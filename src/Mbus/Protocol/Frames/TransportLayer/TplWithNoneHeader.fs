namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithNoneHeaderRaw = {
    Ci: Field<CiFieldTplNoneHeader>
    AplData: Field<ReadOnlyMemory<byte>>
}

module TplWithNoneHeaderRaw =

    let parse (ci: Field<CiFieldTplNoneHeader>) : Parser<TplWithNoneHeaderRaw> =

        parser {
            let! aplData = parseField "APL Data" takeAll

            return { Ci = ci; AplData = aplData }
        }

type TplWithNoneHeader = {
    Ci: Field<CiFieldTplNoneHeader>
    AplData: Field<ReadOnlyMemory<byte>>
}

module TplWithNoneHeader =

    let validate (raw: Field<TplWithNoneHeaderRaw>) : Validation<Field<TplWithNoneHeader>> =

        raw
        |> Field.withValue {
            Ci = raw.Value.Ci
            AplData = raw.Value.AplData
        }
        |> passed

    /// Writes the TPL CI, header and already represented application bytes.
    let write (tpl: TplWithNoneHeader) : Writer<unit> =
        writer {
            do! CiFieldTpl.write (CiFieldTpl.NoneHeader tpl.Ci.Value)
            do! writeMemory tpl.AplData.Value
        }
