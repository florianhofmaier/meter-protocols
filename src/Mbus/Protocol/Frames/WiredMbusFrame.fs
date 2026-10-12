namespace Metering.Mbus.Protocol.Frames.WiredMbus

open Metering.Common.Decoding.Parsers
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames

type WiredMbusFrame =
    | SingleCharacter of Field<unit>
    | FixedLength of Field<FixedLengthFrame>
    | VariableLength of dll: Field<VariableLengthFrame> * linkUserData: LinkUserData

module WiredMbusFrame =

    /// A semantic view of the same parsed frame field, retaining its provenance.
    let asField (frame: WiredMbusFrame) : Field<WiredMbusFrame> =
        match frame with
        | SingleCharacter field -> field |> Field.withValue frame
        | FixedLength field -> field |> Field.withValue frame
        | VariableLength (field, _) -> field |> Field.withValue frame
