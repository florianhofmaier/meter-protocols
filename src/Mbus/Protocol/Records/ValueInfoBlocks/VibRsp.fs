namespace Metering.Mbus.Protocol.Records.ValueInfoBlocks

open Metering.Common.BaseWriters.Core
open Metering.Common.BaseWriters.BinaryWriters

open Metering.Common.Decoding.Parsers
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Records
open Metering.Mbus.Protocol.Records.ValueInfoBlocks.VibCommon

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module VibRsp =

    let validate (raw: Field<InfoBlockRaw>) : Validation<VibRsp> =

        validator {
            let bytes = vibBytes raw
            let first = bytes.Span[0]

            match first with
            | _ when isSpecial anyVif first -> return! failed raw "Any VIF (0x7E) is not supported"

            | _ when isSpecial mfrVif first -> return VibRsp.Mfr bytes

            | _ when isSpecial textVif first ->
                let! text = TextVib.validate raw
                return VibRsp.Text text

            | _ ->
                return!
                    validateNormal
                        RecordError.tryMap
                        "response extension"
                        (fun vif extensions errors ->
                            VibRsp.Normal {
                                Def = vif
                                Ext = extensions
                                Codes = errors
                            })
                        raw
        }

    /// Writes a response VIB.
    let write (vib: VibRsp) : Writer<unit> =
        match vib with
        | VibRsp.Mfr bytes when
            bytes.Length > 0 && bytes.Length <= 11 && (bytes.Span[0] &&& 0x7Fuy) = 0x7Fuy
            ->
            writeChain (writeMemory bytes)
        | VibRsp.Mfr _ ->
            writerError "Manufacturer VIB must start with 7F and contain at most ten VIFEs"
        | VibRsp.Text text when
            not (isNull text)
            && text.Length <= 255
            && Seq.forall (fun c -> int c < 128) text
            ->
            writer {
                do! writeU8 0x7Cuy
                do! writeU8 (byte text.Length)

                do!
                    writeMemory (
                        System.ReadOnlyMemory(
                            text |> System.Text.Encoding.ASCII.GetBytes |> Array.rev
                        )
                    )
            }
        | VibRsp.Text _ -> writerError "Text VIF requires at most 255 ASCII characters"
        | VibRsp.Normal normal ->
            writeChain (
                writer {
                    let invalidMfrPosition =
                        normal.Ext
                        |> List.mapi (fun i ext ->
                            match ext with
                            | MfrVifExt _ -> i <> normal.Ext.Length - 1 || not normal.Codes.IsEmpty
                            | _ -> false)
                        |> List.exists id

                    if invalidMfrPosition then
                        return! writerError "Manufacturer VIFE must terminate the VIB"

                    do! Vif.write normal.Def

                    for ext in normal.Ext do
                        do! VifExtensions.write ext

                    for code in normal.Codes do
                        do! RecordError.write code
                }
            )
