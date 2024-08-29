module Pentole.TestsExtensions

open NUnit.Framework

type NUnit.Framework.Assert with

    static member ok_is_equal<'ok, 'err> (expected: 'ok) (got: Result<'ok, 'err>) =
        match got with
        | Ok got -> Assert.That (got, Is.EqualTo(expected))
        | Error e -> Assert.Fail $"Expected 'ok, got: {e}"

    static member ok_is_true (r: Result<bool, 'a>) =
        match r with
        | Ok got -> Assert.That (got, Is.True)
        | Error e -> Assert.Fail $"Expected truth value, got: {e}"

    static member ok_is_false (r: Result<bool, 'e>) =
        match r with
        | Ok got -> Assert.That (got, Is.False)
        | Error e -> Assert.Fail $"Expected truth value, got: {e}"

    static member are_equal expected (got: 'a) =
        Assert.That (got, Is.EqualTo(expected))

    static member are_seq_equal (expected: 'a seq) (got: 'a seq) =
        Assert.That (got, Is.EquivalentTo(expected))
