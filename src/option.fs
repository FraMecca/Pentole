module Pentole.Option

[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =
    /// <summary>
    /// Chains two option values together.
    /// If the first value is <c>Some</c>, it is returned; otherwise, the second value is returned.
    /// </summary>
    /// <param name="x"></param>
    /// <param name="y"></param>
    let inline coalesce (x : 'T option) (y : 'T option) =
        match x with
        | Some _ -> x
        | None -> y
