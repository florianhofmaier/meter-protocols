module Metering.Common.Security.Cryptography.Tests.AesCbcTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Security.Cryptography
open Metering.Common.Security.Cryptography.AesCbc

let private bytes (hex: string) =
    ReadOnlyMemory<byte>(Convert.FromHexString(hex))

let private secret (hex: string) =
    Secret128.TakeOwnership(Convert.FromHexString(hex))

let private vector (hex: string) =
    match AesCbCIv.create (Convert.FromHexString(hex)) with
    | Ok iv -> iv
    | Error error -> failwith $"Unexpected IV error: %A{error}"

let private expectPlain key iv cipherText =
    let cipher = bytes cipherText

    match AesCbc.decrypt (secret key) (vector iv) cipher 0 cipher.Length with
    | Ok plain -> plain.ToArray()

    | Error error -> failwith $"Unexpected encryption error: %A{error}"

let private nistFourBlockPlaintext =
    Convert.FromHexString(
        "6bc1bee22e409f96e93d7e117393172a"
        + "ae2d8a571e03ac9c9eb76fac45af8e51"
        + "30c81c46a35ce411e5fbc1191a0a52ef"
        + "f69f2445df4f9b17ad2b417be66c3710"
    )

[<Fact>]
let ``valid AES-128-CBC NIST vector decrypts`` () =
    expectPlain
        "2b7e151628aed2a6abf7158809cf4f3c"
        "000102030405060708090a0b0c0d0e0f"
        "7649abac8119b246cee98e9b12e9197d"
    |> should equal (Convert.FromHexString("6bc1bee22e409f96e93d7e117393172a"))

[<Fact>]
let ``AES-192 key is rejected by the AES-128 key type`` () =
    Assert.Throws<ArgumentException>(fun () ->
        secret "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b" |> ignore)
    |> ignore

[<Fact>]
let ``AES-256 key is rejected by the AES-128 key type`` () =
    Assert.Throws<ArgumentException>(fun () ->
        secret "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
        |> ignore)
    |> ignore

[<Fact>]
let ``multiple CBC blocks preserve order and exact output length`` () =
    let plain =
        expectPlain
            "2b7e151628aed2a6abf7158809cf4f3c"
            "000102030405060708090a0b0c0d0e0f"
            "7649abac8119b246cee98e9b12e9197d5086cb9b507219ee95db113a917678b273bed6b8e3c1743b7116e69e222295163ff1caa1681fac09120eca307586e1a7"

    plain |> should equal nistFourBlockPlaintext

    plain.Length |> should equal 64

[<Fact>]
let ``CBC decryption does not add or remove padding`` () =
    let plain =
        expectPlain
            "2b7e151628aed2a6abf7158809cf4f3c"
            "000102030405060708090a0b0c0d0e0f"
            "7649abac8119b246cee98e9b12e9197d"

    plain.Length |> should equal 16

[<Fact>]
let ``empty ciphertext returns empty plaintext for valid key and IV`` () =
    match
        AesCbc.decrypt
            (secret "2b7e151628aed2a6abf7158809cf4f3c")
            (vector "000102030405060708090a0b0c0d0e0f")
            ReadOnlyMemory<byte>.Empty
            0
            0
    with
    | Ok plain -> plain.ToArray() |> should equal [||]

    | Error error -> failwith $"Unexpected encryption error: %A{error}"

[<Fact>]
let ``invalid CBC key lengths are rejected before decryption`` () =
    Assert.Throws<ArgumentException>(fun () -> secret "00" |> ignore) |> ignore

[<Fact>]
let ``invalid CBC IV lengths return validation error`` () =
    for length in [ 0; 1; 15; 17 ] do
        match AesCbCIv.create (Array.zeroCreate length) with
        | Error _ -> ()
        | Ok _ -> failwith $"Accepted invalid IV length {length}"

[<Fact>]
let ``non block-aligned CBC ciphertext returns cryptographic failure`` () =
    match
        AesCbc.decrypt
            (secret "2b7e151628aed2a6abf7158809cf4f3c")
            (vector "000102030405060708090a0b0c0d0e0f")
            (bytes "00")
            0
            1
    with
    | Error error ->
        EncryptionError.value error
        |> should equal "AES-CBC ciphertext length must be a multiple of 16 byte(s)"

    | actual -> failwith $"Expected cryptographic failure, got %A{actual}"

[<Fact>]
let ``CBC invalid ciphertext returns an error rather than throwing`` () =
    let result =
        AesCbc.decrypt
            (secret "2b7e151628aed2a6abf7158809cf4f3c")
            (vector "000102030405060708090a0b0c0d0e0f")
            (bytes "00")
            0
            1

    match result with
    | Error _ -> ()
    | Ok _ -> failwith "Expected invalid ciphertext to be rejected"
