module Metering.Mbus.Protocol.Utility.Bcd

let lowNibble (value: byte) =
    value &&& 0x0Fuy

let highNibble (value: byte) =
    value >>> 4

let isDigit (nibble: byte) =
    nibble <= 9uy

let isDigitOrWildcard (nibble: byte) =
    isDigit nibble || nibble = 0x0Fuy

let tryDecodeByte (value: byte) =
    let low = lowNibble value
    let high = highNibble value

    if isDigit high && isDigit low then
        Some (high * 10uy + low)
    else
        None

let digitShifts digitCount =
    [ 0 .. 4 .. ((digitCount - 1) * 4) ]

let nibbleAt shift (value: uint32) =
    byte ((value >>> shift) &&& 0xFu)

let tryFindInvalidNibble allowWildcard digitCount value =
    digitShifts digitCount
    |> List.tryPick (fun shift ->
        let nibble = nibbleAt shift value
        let isValid =
            if allowWildcard then isDigitOrWildcard nibble else isDigit nibble

        if isValid then None else Some (shift, nibble))

let tryFindInvalidBcdNibble digitCount value =
    tryFindInvalidNibble false digitCount value

let tryFindInvalidBcdOrWildcardNibble digitCount value =
    tryFindInvalidNibble true digitCount value

let decodeUInt32 digitCount value =
    digitShifts digitCount
    |> List.rev
    |> List.fold
        (fun acc shift ->
            acc * 10u + uint32 (nibbleAt shift value))
        0u

let encodeUInt32 digitCount value =
    digitShifts digitCount
    |> List.fold
        (fun (bcd, remaining) shift ->
            let digit = remaining % 10u
            bcd ||| (digit <<< shift), remaining / 10u)
        (0u, value)
    |> fst

let patternAndMaskUInt32 digitCount value =
    digitShifts digitCount
    |> List.fold
        (fun (pattern, mask) shift ->
            let nibble = nibbleAt shift value

            if nibble = 0x0Fuy then
                pattern, mask
            else
                let nibble = uint32 nibble
                pattern ||| (nibble <<< shift), mask ||| (0xFu <<< shift))
        (0u, 0u)

