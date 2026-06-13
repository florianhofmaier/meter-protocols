namespace Metering.Mbus.Protocol.Frames.Tpl

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Parsers.Core
open Metering.Common.Decoding.Parsers.FieldParser
open Metering.Mbus.Protocol.Frames.DeviceIdentification

type TplNoneRaw =
    {
        Ci: ParsedField<NoneHeaderCi>
    }

type ShortHeaderRaw =
    {
        Acc: ParsedField<AccessNumberRaw>
        Status: ParsedField<StatusRaw>
        Cnf: ParsedField<ConfigurationRaw>
    }

module ShortHeaderRaw =

    let parse : Parser<ParsedField<ShortHeaderRaw>> =
        parseField "Short Header"
        <| parser {
            let! acc = AccessNumberRaw.parse
            let! status = StatusRaw.parse
            let! cnf = ConfigurationRaw.parse

            return
                {
                    Acc = acc
                    Status = status
                    Cnf = cnf
                }
        }

type TplShortRaw =
    {
        Ci: ParsedField<ShortHeaderCi>
        Header : ParsedField<ShortHeaderRaw>
    }


type LongHeaderRaw =
    {
        IdNum: ParsedField<IdNumberRaw>
        Mfr: ParsedField<ManufacturerRaw>
        Version: ParsedField<VersionRaw>
        DevType: ParsedField<DeviceTypeRaw>
        Acc: ParsedField<AccessNumberRaw>
        Status: ParsedField<StatusRaw>
        Cnf: ParsedField<ConfigurationRaw>
    }

module LongHeaderRaw =

    let parse : Parser<ParsedField<LongHeaderRaw>> =
        parseField "Long Header"
        <| parser {
            let! idNum = IdNumberRaw.parse
            let! mfr = ManufacturerRaw.parse
            let! version = VersionRaw.parse
            let! devType = DeviceTypeRaw.parse
            let! acc = AccessNumberRaw.parse
            let! status = StatusRaw.parse
            let! cnf = ConfigurationRaw.parse

            return
                {
                    IdNum = idNum
                    Mfr = mfr
                    Version = version
                    DevType = devType
                    Acc = acc
                    Status = status
                    Cnf = cnf
                }
        }

type TplLongRaw =
    {
        Ci: ParsedField<LongHeaderCi>
        Header : ParsedField<LongHeaderRaw>
    }

type TplRaw =
    | NoneHeader of TplNoneRaw
    | ShortHeader of TplShortRaw
    | LongHeader of TplLongRaw

module TplRaw =

    let parse : Parser<ParsedField<TplRaw>> =
        parseField "TPL"
        <| parser {
            let! ci = CiFieldTpl.parse

            match ci.Value with
            | CiFieldTpl.NoneHeader noneCi ->
                return
                    NoneHeader
                        {
                            Ci = ci |> ParsedField.map (fun _ -> noneCi)
                        }

            | CiFieldTpl.ShortHeader shortCi ->
                let! header = ShortHeaderRaw.parse
                return
                    ShortHeader
                        {
                            Ci = ci |> ParsedField.map (fun _ -> shortCi)
                            Header = header
                        }

            | CiFieldTpl.LongHeader longCi ->
                let! header = LongHeaderRaw.parse
                return
                    LongHeader
                        {
                            Ci = ci |> ParsedField.map (fun _ -> longCi)
                            Header = header
                        }
        }