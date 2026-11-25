namespace Metering.Mbus.Protocol.Messages

open Metering.Common.BaseWriters.BinaryWriters
open Metering.Mbus.Protocol.Frames
open Metering.Mbus.Protocol.Frames.ApplicationLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus
open Metering.Mbus.Protocol.Frames.DeviceIdentification
open Metering.Mbus.Protocol.Frames.TransportLayer

type ResponseAlarms = {
    Address: AField
    Acd: bool
    Dfc: bool
    Status: StatusByte
    MeterAddress: MeterAddress
    Alarms: Alarms
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ResponseAlarms =
    /// Writes the alarm application payload without selecting protection.
    let writePayload (message: ResponseAlarms) = Alarms.write message.Alarms

    /// Composes an alarm response using the encoder's resolved TPL header and payload.
    let write header payload (message: ResponseAlarms) =
        VariableLengthFrame.write
            (CField.writeSecondary message.Acd message.Dfc)
            message.Address
            (TplWithLongHeader.writeWithHeader
                CiFieldTplLongHeader.Alarm
                header
                (writeMemory payload))
