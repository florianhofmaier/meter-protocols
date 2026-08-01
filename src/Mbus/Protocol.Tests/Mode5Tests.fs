module Metering.Mbus.Protocol.Tests.Mode5Tests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Decoding.ByteReaders
open Metering.Common.Decoding.Parsers.ParserRunner
open Metering.Common.Decoding.Parsers.Types
open Metering.Common.Decoding.Validators.Core
open Metering.Mbus.Protocol.Frames.TransportLayer
open Metering.Mbus.Protocol.Frames.TransportLayer.Security
open Metering.Mbus.Protocol.Tests.TestSupport

type CapturingSourceStore() =
    let mutable derivedBytes : ReadOnlyMemory<byte> option = None
    let mutable derivedSource : SourceInfo option = None

    member _.DerivedBytes = derivedBytes
    member _.DerivedSource = derivedSource

    interface ISourceStore with
        member _.AddRoot name bytes sensitive =
            {
                Id = SourceId.create 100
                Name = name
                Origin = None
                Transform = SourceTransform.Root
                Length = bytes.Length
                Sensitive = sensitive
            }

        member _.AddDerived name origin transform bytes sensitive =
            let source =
                {
                    Id = SourceId.create 101
                    Name = name
                    Origin = Some origin
                    Transform = transform
                    Length = bytes.Length
                    Sensitive = sensitive
                }

            derivedBytes <- Some bytes
            derivedSource <- Some source
            source

        member _.CreateReader _ =
            ByteReaderFactory.Create(ReadOnlyMemory<byte>.Empty, 0)

let private validKey =
    [| 0x00uy .. 0x0Fuy |]

let private mode5Context keyBytes =
    match Mode5Key.create (memory keyBytes) with
    | Ok key -> SecurityContext.mode5 key
    | Error error -> failwith $"Invalid test key: %A{error}"

let private header encryptedLength =
    let cnf =
        match encryptedLength with
        | NoEncryptedData -> 0x0500us
        | FixedEncryptedBlocks count ->
            0x0500us ||| (uint16 (EncryptedBlockCount.value count) <<< 4)
        | AllRemainingDataEncrypted -> 0x05F0us

    let raw =
        parseExactly
            LongHeaderRaw.parse
            [|
                0x02uy; 0x03uy; 0x04uy; 0x05uy
                0x00uy; 0x01uy
                0x06uy
                0x07uy
                0x08uy
                0x00uy
                byte cnf; byte (cnf >>> 8)
            |]

    match raw.Value with
    | LongHeaderRaw.Mode5Raw header -> header
    | actual -> failwith $"Expected Mode 5 header, got %A{actual}"

let private fixedBlocks count =
    match EncryptedBlockCount.tryCreate count with
    | Some value -> FixedEncryptedBlocks value
    | None -> failwith $"Invalid block count {count}"

let private run context encryptedLength payload =
    let store = CapturingSourceStore()

    let result =
        Metering.Common.Decoding.Parsers.ParserRunner.run
            (ByteReaderFactory.Create(ReadOnlyMemory<byte>.Empty, 0))
            (store :> ISourceStore)
            trace
            (Mode5.expandLongHeader
                context
                (header encryptedLength)
                (bytesField payload))

    result, store

let private outcome =
    function
    | Ok value -> value
    | actual -> failwith $"Expected Mode 5 outcome, got %A{actual}"

let private failures =
    function
    | Mode5ExpansionRaw.InvalidProtectionLayout failures ->
        failures
        |> Failures.toList
        |> List.map (fun issue -> issue.Message)
    | actual -> failwith $"Expected invalid outcome, got %A{actual}"

let private validCipherText =
    [|
        0x26uy; 0x06uy; 0xA6uy; 0xF4uy
        0xEDuy; 0x1Auy; 0x1Buy; 0x2Buy
        0x5Cuy; 0x89uy; 0x1Auy; 0xAEuy
        0x6Duy; 0x50uy; 0x8Fuy; 0x01uy
    |]

let private expectedPlainPrefix =
    [|
        0x0Cuy; 0x13uy; 0x37uy; 0x00uy
        0x01uy; 0x02uy; 0x03uy; 0x04uy
        0x05uy; 0x06uy; 0x07uy; 0x08uy
        0x09uy; 0x0Auy
    |]

[<Fact>]
let ``no security context retains protected layout`` () =
    let result, _ =
        run SecurityContext.none (fixedBlocks 1) validCipherText

    match outcome result with
    | Mode5ExpansionRaw.Protected protectedApl ->
        protectedApl.Failure |> should equal UnprotectionIssue.SecurityContextNotUsable
        protectedApl.OriginalPayload.Value.ToArray() |> should equal validCipherText
        protectedApl.EncryptedPart.Value.Length |> should equal 16
        protectedApl.ClearSuffix |> should equal None
    | actual -> failwith $"Expected protected outcome, got %A{actual}"

[<Fact>]
let ``wrong key is a protected cryptographic failure`` () =
    let wrongKey = Array.create 16 0xAAuy
    let result, _ =
        run (mode5Context wrongKey) AllRemainingDataEncrypted validCipherText

    match outcome result with
    | Mode5ExpansionRaw.Protected {
        Failure = UnprotectionIssue.CryptographicFailure
            CryptographicFailure.DecryptionOrVerificationFailed
      } -> ()
    | actual -> failwith $"Expected cryptographic protected outcome, got %A{actual}"

[<Fact>]
let ``partial encryption combines decrypted prefix and clear suffix`` () =
    let suffix = [| 0x2Fuy; 0x2Fuy |]
    let payload = Array.append validCipherText suffix
    let result, store =
        run (mode5Context validKey) (fixedBlocks 1) payload

    match outcome result with
    | Mode5ExpansionRaw.Unprotected source ->
        source.Value.ToArray()
        |> should equal (Array.append expectedPlainPrefix suffix)
        source.Span.Source |> should equal (SourceId.create 101)
        store.DerivedBytes.IsSome |> should be True
    | actual -> failwith $"Expected unprotected partial payload, got %A{actual}"

[<Fact>]
let ``failed partial unprotection retains clear suffix without deriving a source`` () =
    let suffix = [| 0x0Fuy |]
    let payload = Array.append (Array.zeroCreate 16) suffix
    let result, store =
        run (mode5Context validKey) (fixedBlocks 1) payload

    match outcome result with
    | Mode5ExpansionRaw.Protected protectedApl ->
        protectedApl.EncryptedPart.Value.Length |> should equal 16
        protectedApl.ClearSuffix.Value.Value.ToArray() |> should equal suffix
        store.DerivedSource |> should equal None
    | actual -> failwith $"Expected retained protected layout, got %A{actual}"

[<Fact>]
let ``zero encrypted blocks returns the original unprotected source`` () =
    let payload = [| 0x2Fuy; 0x2Fuy |]
    let result, store =
        run SecurityContext.none NoEncryptedData payload

    match outcome result with
    | Mode5ExpansionRaw.Unprotected source ->
        source.Value.ToArray() |> should equal payload
        source.Span |> should equal (bytesField payload).Span
        store.DerivedSource |> should equal None
    | actual -> failwith $"Expected original source, got %A{actual}"

[<Fact>]
let ``all remaining is not capped at fifteen blocks`` () =
    let cipherText =
        Convert.FromHexString(
            "aaa81ab26bb285ae3da10ad481a1ac2c04f32cbe18ae843d54e7ff4c7d948a74"
            + "91a745434dd79ad4c5695053601c5928d3d76cb55bea56808012a127ec4d1f58"
            + "fdd2416e73e931a1b902fec138bb27a9e14b08dcb9b46716958592028081d27"
            + "f600fa30e803d8094ecd736f8e294c101d9d0ebc77520a04e80b243788e8a"
            + "2dafb64f8e0cb2fd0a38d650b4020ebb9e1adba581e288583f36e39e1b2c"
            + "6b2f636de9bbf80b6f2943409c40ceab098cc301654433173abdbc1f6e59e"
            + "541be1111f2ae4d4ebc13850c026159ce4e09c44722e1d50507149fbbaefe"
            + "675331f1cc88eaeb2594b4578c65854a117df0a21b3934685994f2694282"
            + "a84a82504d6f2e99fd"
        )

    let result, _ =
        run (mode5Context validKey) AllRemainingDataEncrypted cipherText

    match outcome result with
    | Mode5ExpansionRaw.Unprotected source ->
        source.Value.Length |> should equal 254
    | actual -> failwith $"Expected all 256 encrypted bytes, got %A{actual}"

[<Fact>]
let ``invalid protected length is invalid rather than protected`` () =
    let result, _ =
        run SecurityContext.none AllRemainingDataEncrypted (Array.zeroCreate 17)

    outcome result
    |> failures
    |> List.exists (fun message -> message.Contains("multiple of 16"))
    |> should be True

[<Fact>]
let ``fixed encrypted prefix larger than payload is invalid`` () =
    let result, _ =
        run SecurityContext.none (fixedBlocks 2) (Array.zeroCreate 31)

    outcome result
    |> failures
    |> List.exists (fun message ->
        message.Contains("32 encrypted byte(s)")
        && message.Contains("31 payload byte(s)"))
    |> should be True
