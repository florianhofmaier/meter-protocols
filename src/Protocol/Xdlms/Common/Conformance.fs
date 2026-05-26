namespace Metering.Dlms.Protocol.Xdlms

open Metering.Common.Parsers.BinaryParsers
open Metering.Common.Parsers.Core
open Metering.Common.Parsers.ParserTree
open Metering.Common.Validators.Core
open Metering.Dlms.Protocol

type ConformanceRaw =
    private
        ConformanceRaw of Ber.BitString

module ConformanceRaw =
    let value (ConformanceRaw bits) = bits

    let parse : Parser<Parsed<ConformanceRaw>> =
        parseNode "Conformance" <|
        parser {
            do! expectU16BigEndian 0x5F1Fus
            return! Ber.BitString.parseImplicit |>> ConformanceRaw
        }

type Conformance =
    private Conformance of Ber.BitString

module Conformance =
    let validate (raw: ConformanceRaw) : Validation<Conformance> =
        validator {
            let bits = ConformanceRaw.value raw

            do!
                Validation.ensure
                    "conformance unused bit count must be 0"
                    (bits.UnusedBitCount = 0uy)

            do!
                Validation.ensure
                    "conformance must contain exactly 24 bits / 3 octets"
                    (bits.Payload.Length = 3)

            return Conformance bits
        }

    let private isSet bit (Conformance bits) =
        let byteIndex = bit / 8
        let bitIndex = bit % 8
        let mask = 0x80uy >>> bitIndex

        (bits.Payload.Span[byteIndex] &&& mask) <> 0uy

    let generalProtection c = isSet 1 c
    let generalBlockTransfer c = isSet 2 c

    let read c = isSet 3 c
    let write c = isSet 4 c
    let unconfirmedWrite c = isSet 5 c
    let deltaValueEncoding c = isSet 6 c

    let attribute0SupportedWithSet c = isSet 8 c
    let priorityMgmtSupported c = isSet 9 c
    let attribute0SupportedWithGet c = isSet 10 c
    let blockTransferWithGetOrRead c = isSet 11 c
    let blockTransferWithSetOrWrite c = isSet 12 c
    let blockTransferWithAction c = isSet 13 c
    let multipleReferences c = isSet 14 c
    let informationReport c = isSet 15 c
    let dataNotification c = isSet 16 c
    let access c = isSet 17 c
    let parameterizedAccess c = isSet 18 c
    let get c = isSet 19 c
    let set c = isSet 20 c
    let selectiveAccess c = isSet 21 c
    let eventNotification c = isSet 22 c
    let action c = isSet 23 c