module Metering.Common.Security.Cryptography.Tests.AesGcmTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Security.Cryptography

let private bytes (hex: string) =
    ReadOnlyMemory<byte>(Convert.FromHexString(hex))

let private expectPlain key nonce associatedData cipherText tag =
    match AesGcm.decrypt (bytes key) (bytes nonce) (bytes associatedData) (bytes cipherText) (bytes tag) with
    | Ok plain ->
        plain.ToArray()

    | Error error ->
        failwith $"Unexpected encryption error: %A{error}"

let private gcmKey =
    "00000000000000000000000000000000"

let private gcmNonce =
    "000000000000000000000000"

let private gcmCipherText =
    "0388dace60b6a392f328c2b971b2fe78"

let private gcmTag96 =
    "ab6e47d42cec13bdf53a67b2"

let private aadKey =
    "feffe9928665731c6d6a8f9467308308"

let private aadNonce =
    "cafebabefacedbaddecaf888"

let private aad =
    "feedfacedeadbeeffeedfacedeadbeefabaddad2"

let private aadPlain =
    "d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39"

let private aadCipher =
    "42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091"

let private aadTag96 =
    "5bc94fbc3221a5db94fae95a"

[<Fact>]
let ``valid GCM vector decrypts without associated data`` () =
    expectPlain gcmKey gcmNonce "" gcmCipherText gcmTag96
    |> should equal (Convert.FromHexString("00000000000000000000000000000000"))

[<Fact>]
let ``valid GCM vector decrypts with associated data`` () =
    expectPlain aadKey aadNonce aad aadCipher aadTag96
    |> should equal (Convert.FromHexString(aadPlain))

[<Fact>]
let ``verifyTag accepts empty plaintext tag vector`` () =
    match AesGcm.verifyTag (bytes gcmKey) (bytes gcmNonce) ReadOnlyMemory<byte>.Empty (bytes "58e2fccefa7e3061367f1d57") with
    | Ok () ->
        ()

    | Error error ->
        failwith $"Unexpected encryption error: %A{error}"

[<Fact>]
let ``modified ciphertext returns authentication failure`` () =
    match AesGcm.decrypt (bytes gcmKey) (bytes gcmNonce) ReadOnlyMemory<byte>.Empty (bytes "0388dace60b6a392f328c2b971b2fe79") (bytes gcmTag96) with
    | Error AuthenticationFailed ->
        ()

    | actual ->
        failwith $"Expected authentication failure, got %A{actual}"

[<Fact>]
let ``modified tag returns authentication failure`` () =
    match AesGcm.decrypt (bytes gcmKey) (bytes gcmNonce) ReadOnlyMemory<byte>.Empty (bytes gcmCipherText) (bytes "ab6e47d42cec13bdf53a67b3") with
    | Error AuthenticationFailed ->
        ()

    | actual ->
        failwith $"Expected authentication failure, got %A{actual}"

[<Fact>]
let ``modified associated data returns authentication failure`` () =
    match AesGcm.decrypt (bytes aadKey) (bytes aadNonce) (bytes "feedfacedeadbeeffeedfacedeadbeefabaddad3") (bytes aadCipher) (bytes aadTag96) with
    | Error AuthenticationFailed ->
        ()

    | actual ->
        failwith $"Expected authentication failure, got %A{actual}"

[<Fact>]
let ``invalid GCM key nonce and tag lengths return validation errors`` () =
    match AesGcm.decrypt (bytes "00") (bytes gcmNonce) ReadOnlyMemory<byte>.Empty (bytes gcmCipherText) (bytes gcmTag96) with
    | Error (InvalidKeyLength length) ->
        length |> should equal 1

    | actual ->
        failwith $"Expected invalid key length, got %A{actual}"

    match AesGcm.decrypt (bytes gcmKey) (bytes "00") ReadOnlyMemory<byte>.Empty (bytes gcmCipherText) (bytes gcmTag96) with
    | Error (InvalidNonceLength length) ->
        length |> should equal 1

    | actual ->
        failwith $"Expected invalid nonce length, got %A{actual}"

    match AesGcm.decrypt (bytes gcmKey) (bytes gcmNonce) ReadOnlyMemory<byte>.Empty (bytes gcmCipherText) (bytes "00") with
    | Error (InvalidTagLength length) ->
        length |> should equal 1

    | actual ->
        failwith $"Expected invalid tag length, got %A{actual}"

[<Fact>]
let ``GCM output length equals ciphertext length`` () =
    let plain =
        expectPlain gcmKey gcmNonce "" gcmCipherText gcmTag96

    plain.Length |> should equal (Convert.FromHexString(gcmCipherText).Length)

[<Fact>]
let ``GCM cryptographic failures are returned rather than thrown`` () =
    let result =
        AesGcm.decrypt (bytes gcmKey) (bytes gcmNonce) ReadOnlyMemory<byte>.Empty (bytes gcmCipherText) (bytes "ab6e47d42cec13bdf53a67b3")

    match result with
    | Error AuthenticationFailed ->
        ()

    | actual ->
        failwith $"Expected authentication failure, got %A{actual}"
