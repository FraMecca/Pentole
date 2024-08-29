module Pentole.Result

type Result<'o, 'e> with
    
    /// <summary>
    /// `o` if `Ok o` else throw
    /// </summary>
    /// <param name="result"></param>
    static member get (result: Result<'o, 'e>) =
        match result with
        | Ok o -> o
        | Error e -> failwith $"Can't unwrap Error {e}"
