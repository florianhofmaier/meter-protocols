namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type UserInformation =
    private
        UserInformation of Ber.OctetString

module UserInformation =

    let create value =
        UserInformation value

    let value (UserInformation value) =
        Ber.OctetString.toBytes value

    let validate
        (raw: Ber.OctetString)
        : Validation<UserInformation> =

        if Ber.OctetString.length raw > 0 then
            Validation.ok (UserInformation raw)

        else
            Validation.error "user-information is empty"