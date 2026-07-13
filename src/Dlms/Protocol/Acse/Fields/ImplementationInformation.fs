namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Dlms.Protocol

type ImplementationInformation =
    private
        ImplementationInformation of Ber.GraphicString

module ImplementationInformation =

    let create value =
        ImplementationInformation value

    let value (ImplementationInformation value) =
        value

    let validate (raw: ParsedField<Ber.GraphicString>) =
        raw.Value |> create |> passed