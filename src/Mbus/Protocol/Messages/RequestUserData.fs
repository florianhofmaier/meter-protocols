namespace Metering.Mbus.Protocol.Messages

open Metering.Mbus.Protocol.Frames.DataLinkLayer.WiredMbus

module RequestUserData =

    let private addressFromAField =
        function
        | AField.Unconfigured ->
            Some RequestUserDataAddress.Unconfigured

        | AField.Configured address ->
            Some (RequestUserDataAddress.Configured address)

        | AField.SelectionOfDevice ->
            Some RequestUserDataAddress.SelectionOfDevice

        | AField.Diagnosis ->
            Some RequestUserDataAddress.Diagnosis

        | AField.RepeaterMgmt
        | AField.Broadcast ->
            None

    let tryFromFrame =
        function
        | Frame.FixedLength
            {
                CField = CField.Primary
                    {
                        Fcb = fcb
                        Fcv = true
                        Func = PrimaryFunction.RequestUserDataClass2
                    }
                AField = aField
            } ->
            addressFromAField aField
            |> Option.map (fun address ->
                RequestUserData {
                    Fcb = fcb
                    Address = address
                })

        | _ ->
            None
