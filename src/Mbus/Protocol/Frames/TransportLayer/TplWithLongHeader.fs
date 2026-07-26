namespace Metering.Mbus.Protocol.Frames.TransportLayer

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames

type TplWithLongHeaderRaw =
    {
        Ci: Field<CiFieldTplLongHeader>
        Header: Field<LongHeaderRaw>
        AplData: Field<AplDataRaw>
    }

module TplWithLongHeaderRaw =

    let parse ci : Parser<TplWithLongHeaderRaw> =
        parser {
            let! header = LongHeaderRaw.parse
            let! aplData = AplDataRaw.parse

            return {
                Ci = ci
                Header = header
                AplData = aplData
            }
        }

type TplWithLongHeader =
    {
        Ci: Field<CiFieldTplLongHeader>
        Header: Field<LongHeader>
    }

module TplWithLongHeader =

    let fromRaw
        (raw: TplWithLongHeaderRaw)
        : Validation<TplWithLongHeader> =

        validator {
            let! header = LongHeader.fromRaw raw.Header

            return
                {
                    Ci = raw.Ci
                    Header = header
                }
        }
