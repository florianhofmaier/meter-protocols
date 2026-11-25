namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer
open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

type SendUserData2 = {
    Address: AField
    Data: LinkUserDataBytes
}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module SendUserData2 =
    let write (message: SendUserData2) =
        VariableLengthFrame.write
            (CField.writePrimary PrimaryFunction.SendUserData false false)
            message.Address
            (LinkUserDataBytes.write message.Data)
