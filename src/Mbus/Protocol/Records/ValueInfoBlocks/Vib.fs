namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open System
open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records

type NormalRspVib = { Def: Vif; Ext: VifExtension list; Codes: RecordError list }

type NormalCmdVib = { Def: Vif; Ext: VifExtension list; Actions: ActionCode list }

type VibRsp =
    | Mfr of ReadOnlyMemory<uint8>
    | Normal of NormalRspVib
    | Text of string

type VibCmd =
    | Normal of NormalCmdVib

type VibReq = VibCmd

module internal VibCommon =

    let mask = 0x7Fuy
    let firstExtension = 0xFBuy
    let secondExtension = 0xFDuy
    let textVif = 0x7Cuy

    let thirdExtension = 0xEFuy
    let anyVif = 0x7Euy
    let mfrVif = 0x7Fuy

    let isSpecial expected (b: byte) =
        (b &&& mask) = expected

    type ExtensionScan<'code> =
        {
            Pos: int
            Extensions: VifExtension list
            Codes: 'code list
        }

    let vibBytes (raw: ParsedField<InfoBlockRaw>) =
        InfoBlockRaw.bytes raw.Value

    let hex (bytes: ReadOnlyMemory<byte>) =
        Convert.ToHexString bytes.Span

    let issue raw message : Validation<unit> =
        failed raw message

    let private invalidVifExtension
        (raw: ParsedField<InfoBlockRaw>)
        (bytes: ReadOnlyMemory<byte>)
        (pos: int)
        (nextPos: int)
        : Validation<'a> =

        let invalidPos =
            if isSpecial textVif bytes.Span[pos] && nextPos > pos + 1 && pos + 1 < bytes.Length then
                pos + 1
            else
                pos

        failed raw $"Invalid VIB extension byte 0x{bytes.Span[invalidPos]:X2} at index {invalidPos} in VIB {hex bytes}"

    let private unknownCode
        (raw: ParsedField<InfoBlockRaw>)
        (bytes: ReadOnlyMemory<byte>)
        (unknownCodeDescription: string)
        (pos: int)
        : Validation<'a> =

        failed raw $"Unknown VIB {unknownCodeDescription} byte 0x{bytes.Span[pos]:X2} at index {pos} in VIB {hex bytes}"

    let private emptyScan start : ExtensionScan<'code> =
        {
            Pos = start
            Extensions = []
            Codes = []
        }

    let scanExtensions
        (tryMapCode: ReadOnlyMemory<byte> -> int -> 'code option * int)
        (unknownCodeDescription: string)
        (raw: ParsedField<InfoBlockRaw>)
        (start: int)
        : Validation<ExtensionScan<'code>> =

        let bytes = vibBytes raw

        let rec loop (state: ExtensionScan<'code>) =
            if state.Pos >= bytes.Length then
                passed state
            else
                let ext, nextPos = VifExtensions.tryMap bytes state.Pos

                match ext with
                | Some ext ->
                    loop { state with Pos = nextPos; Extensions = ext :: state.Extensions }

                | _ when nextPos > state.Pos ->
                    invalidVifExtension raw bytes state.Pos nextPos

                | _ ->
                    let code, nextPos = tryMapCode bytes state.Pos

                    match code with
                    | Some code ->
                        loop { state with Pos = nextPos; Codes = code :: state.Codes }

                    | _ ->
                        unknownCode raw bytes unknownCodeDescription state.Pos

        loop (emptyScan start)

    let validateNormal
        (tryMapCode: ReadOnlyMemory<byte> -> int -> 'code option * int)
        (unknownCodeDescription: string)
        (build: Vif -> VifExtension list -> 'code list -> 'result)
        (raw: ParsedField<InfoBlockRaw>)
        : Validation<'result> =

        validator {
            let vifValidation, pos = Vif.map raw 0
            let! vif = vifValidation

            let! scan = scanExtensions tryMapCode unknownCodeDescription raw pos

            return
                build
                    vif
                    (List.rev scan.Extensions)
                    (List.rev scan.Codes)
        }
