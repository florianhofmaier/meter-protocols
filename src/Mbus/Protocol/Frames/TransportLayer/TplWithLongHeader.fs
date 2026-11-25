namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Common.Decoding.Parsers.Utility
open Metering.Common.Decoding.Validators.Core
open Metering.Common.Decoding.Validators.Utility
open Metering.Mbus.Protocol.Frames

type TplWithLongHeaderRaw = {
    Ci: Field<CiFieldTplLongHeader>
    Header: Field<LongHeaderRaw>
    AplData: Field<ReadOnlyMemory<byte>>
}

module TplWithLongHeaderRaw =

    let parse (ci: Field<CiFieldTplLongHeader>) : Parser<TplWithLongHeaderRaw> =

        parser {
            let! header = LongHeaderRaw.parse
            let! aplData = parseField "APL Data" takeAll

            return {
                Ci = ci
                Header = header
                AplData = aplData
            }
        }

type TplWithLongHeader = {
    Ci: Field<CiFieldTplLongHeader>
    Header: Field<LongHeader>
    AplData: Field<ReadOnlyMemory<byte>>
}

module TplWithLongHeader =

    let validate (raw: Field<TplWithLongHeaderRaw>) : Validation<Field<TplWithLongHeader>> =

        validateField LongHeader.validate raw.Value.Header
        |> map (fun header ->
            raw
            |> Field.withValue {
                Ci = raw.Value.Ci
                Header = header
                AplData = raw.Value.AplData
            })

    /// Composes the long-header TPL variant from its CI, header writer and payload writer.
    let internal writeWithHeader ci header payload : Writer<unit> =
        writer {
            do! CiFieldTpl.write (CiFieldTpl.LongHeader ci)
            do! header
            do! payload
        }

    /// Writes the TPL CI, header and already represented application bytes.
    let write (tpl: TplWithLongHeader) : Writer<unit> =
        writeWithHeader
            tpl.Ci.Value
            (LongHeader.write tpl.Header.Value)
            (writeMemory tpl.AplData.Value)
