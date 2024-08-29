module Tests.String

open NUnit.Framework

open Pentole.TestsExtensions
open Pentole

[<Test>]
let split_test () =
    let target = "a/b/c"
    Assert.are_seq_equal (target.Split("/")) (String.split "/" target)
    
