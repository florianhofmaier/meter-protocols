namespace Metering.Dlms.Protocol.Acse.Fields

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
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
        (raw: ParsedField<Ber.OctetString>)
        : Validation<UserInformation> =

        if Ber.OctetString.length raw.Value > 0 then
            passed(UserInformation raw.Value)

        else
            failed raw "user-information is empty"