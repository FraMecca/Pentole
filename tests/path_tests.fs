module Tests.Path

open NUnit.Framework

open Pentole.TestsExtensions
open Pentole.Path
open Pentole.Result

[<Test>]
let absolute_test () =
    let is_absolute = function
        | (Absolute path | Relative path) ->
            System.IO.Path.IsPathFullyQualified path

    let test (s:string) =
        s
        |> Path.of_string
        |> Result.map is_absolute
             
    "/"   |>   test   |> Assert.ok_is_true
    "//"  |>   test   |> Assert.ok_is_true
    "tmp/" |> test |> Assert.ok_is_false
    "tmp"  |> test |> Assert.ok_is_false
    "~"   |>   test   |> Assert.ok_is_false
    "."   |>   test   |> Assert.ok_is_false
    ".."  |>   test   |> Assert.ok_is_false
    "../" |>   test   |> Assert.ok_is_false
    "../../" |> test |> Assert.ok_is_false
    "..a" |>   test   |> Assert.ok_is_false
    ":a"  |>   test   |> Assert.ok_is_false
    "\\a" |>   test   |> Assert.ok_is_false
    "/etc/../" |>   test   |> Assert.ok_is_true
    "/tmp/Char:é" |>   test   |> Assert.ok_is_true

[<Test>]
let resolve_test () =
    let test (s:string) =
        s
        |> Path.of_string
        |> Result.bind resolve
    let p (n: string ) = Path.of_string n |> Result.get

    "/"            |> test |> Assert.ok_is_equal (p "/")
    "/etc/../"     |> test |> Assert.ok_is_equal (p "/")
    "/etc/../etc"  |> test |> Assert.ok_is_equal (p "/etc")

[<Test>]
let equality_test () =
    let p (n: string ) = Path.of_string n |> Result.get
    Assert.are_equal (p "/etc") (p "/etc/")
    
[<Test>]
let parent_test () =
    let p (n: string ) = Path.of_string n |> Result.get
    let test (s:string) =
        s
        |> Path.of_string
        |> Result.get
        |> parent

    "/"            |> test |> Assert.are_equal [p "/"]
    "/etc/"     |> test |> Assert.are_seq_equal ["/"]
    "/etc/conf"     |> test |> Assert.are_seq_equal [p "/etc"]
    "/etc/../etc"  |> test |> Assert.ok_is_equal (p "/etc")
