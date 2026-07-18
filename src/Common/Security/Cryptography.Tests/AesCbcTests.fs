module Metering.Common.Security.Cryptography.Tests.AesCbcTests

open System
open Xunit
open FsUnit.Xunit
open Metering.Common.Security.Cryptography

let private bytes (hex: string) =
    ReadOnlyMemory<byte>(Convert.FromHexString(hex))

let private expectPlain key iv cipherText =
    match AesCbc.decrypt (bytes key) (bytes iv) (bytes cipherText) with
    | Ok plain ->
        plain.ToArray()

    | Error error ->
        failwith $"Unexpected encryption error: %A{error}"

[<Fact>]
let ``valid AES-128-CBC NIST vector decrypts`` () =
    expectPlain
        "2b7e151628aed2a6abf7158809cf4f3c"
        "000102030405060708090a0b0c0d0e0f"
        "7649abac8119b246cee98e9b12e9197d"
    |> should equal (Convert.FromHexString("6bc1bee22e409f96e93d7e117393172a"))

[<Fact>]
let ``valid AES-192-CBC NIST vector decrypts`` () =
    expectPlain
        "8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b"
        "000102030405060708090a0b0c0d0e0f"
        "4f021db243bc633d7178183a9fa071e8"
    |> should equal (Convert.FromHexString("6bc1bee22e409f96e93d7e117393172a"))

[<Fact>]
let ``valid AES-256-CBC NIST vector decrypts`` () =
    expectPlain
        "603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4"
        "000102030405060708090a0b0c0d0e0f"
        "f58c4c04d6e5f1ba779eabfb5f7bfbd6"
    |> should equal (Convert.FromHexString("6bc1bee22e409f96e93d7e117393172a"))

[<Fact>]
let ``multiple CBC blocks preserve order and exact output length`` () =
    let plain =
        expectPlain
            "2b7e151628aed2a6abf7158809cf4f3c"
            "000102030405060708090a0b0c0d0e0f"
            "7649abac8119b246cee98e9b12e9197d5086cb9b507219ee95db113a917678b273bed6b8e3c1743b7116e69e222295163ff1caa1681fac09120eca307586e1a7"

    plain
    |> should equal (Convert.FromHexString("6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710"))

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
    match AesCbc.decrypt (bytes "2b7e151628aed2a6abf7158809cf4f3c") (bytes "000102030405060708090a0b0c0d0e0f") ReadOnlyMemory<byte>.Empty with
    | Ok plain ->
        plain.ToArray() |> should equal [||]

    | Error error ->
        failwith $"Unexpected encryption error: %A{error}"

[<Fact>]
let ``invalid CBC key lengths return validation error`` () =
    match AesCbc.decrypt (bytes "00") (bytes "000102030405060708090a0b0c0d0e0f") (bytes "7649abac8119b246cee98e9b12e9197d") with
    | Error (InvalidKeyLength length) ->
        length |> should equal 1

    | actual ->
        failwith $"Expected invalid key length, got %A{actual}"

[<Fact>]
let ``invalid CBC IV lengths return validation error`` () =
    match AesCbc.decrypt (bytes "2b7e151628aed2a6abf7158809cf4f3c") (bytes "00") (bytes "7649abac8119b246cee98e9b12e9197d") with
    | Error (InvalidInitializationVectorLength length) ->
        length |> should equal 1

    | actual ->
        failwith $"Expected invalid IV length, got %A{actual}"

[<Fact>]
let ``non block-aligned CBC ciphertext returns cryptographic failure`` () =
    match AesCbc.decrypt (bytes "2b7e151628aed2a6abf7158809cf4f3c") (bytes "000102030405060708090a0b0c0d0e0f") (bytes "00") with
    | Error (CryptographicFailure message) ->
        message |> should equal "AES-CBC ciphertext length must be a multiple of 16 byte(s)"

    | actual ->
        failwith $"Expected cryptographic failure, got %A{actual}"

[<Fact>]
let ``CBC validation errors are returned rather than thrown`` () =
    let result =
        AesCbc.decrypt (bytes "00") (bytes "00") (bytes "00")

    match result with
    | Error (InvalidKeyLength length) ->
        length |> should equal 1

    | actual ->
        failwith $"Expected invalid key length, got %A{actual}"
