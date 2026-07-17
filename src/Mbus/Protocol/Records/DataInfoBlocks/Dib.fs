namespace Metering.Mbus.Protocol.Records.DataInfoBlocks

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records

type Dib =
    {
        Fn: FunctionField
        StNum: StorageNumber
        Tariff: Tariff
        SubUnit: SubUnit
    }

module Dib =

    let fromRaw
        (raw: Field<InfoBlockRaw>)
        : Validation<Dib> =

        let bytes = InfoBlockRaw.bytes raw.Value
        let dif = InfoBlockRaw.firstByte raw.Value

        passed
            {
                Fn = FunctionField.fromDif dif
                StNum = StorageNumber.fromInfoBlock bytes
                Tariff = Tariff.fromBytes bytes
                SubUnit = SubUnit.fromBytes bytes
            }
