module Pentole.Path


let private invalid_chars = System.IO.Path.GetInvalidPathChars()

type Path = | Absolute of string | Relative of string
with
    static member of_string (path: string) =
        let is_absolute (path: string) =
            System.IO.Path.IsPathFullyQualified path


        if System.String.IsNullOrEmpty path then
            Error "Can't create a path from an empty string"
        elif invalid_chars |> Seq.exists (fun c -> path.Contains c) then
            Error $"The string contains invalid characters: {path}"
        elif is_absolute path then
            path
            |> System.IO.Path.TrimEndingDirectorySeparator
            |> Absolute
            |> Ok
        else
            path
            |> System.IO.Path.TrimEndingDirectorySeparator
            |> Relative
            |> Ok

let private string_apply fun_ = function
    | Absolute p -> fun_ p |> Absolute
    | Relative p -> fun_ p |> Relative

    /// <summary>
    /// Return the parent path
    /// </summary>
    /// <param name="path"></param>
let parent (path: Path) =
    failwith "t"

/// <summary>
/// Return the extension of the given path
/// </summary>
/// <param name="path"></param>
let extension = function
    | Absolute p ->System.IO.Path.GetExtension  p
    | Relative p ->System.IO.Path.GetExtension  p

/// <summary>
/// Return the basename of the given path
/// </summary>
/// <param name="path"></param>
let basename = function
    | Absolute p ->System.IO.Path.GetFileNameWithoutExtension p
    | Relative p ->System.IO.Path.GetFileNameWithoutExtension p

/// <summary>
/// Concatenates `path` and `other` and adds a directory separator character between any of the path components if one is not already present.
/// </summary>
/// <param name="path"></param>
/// <param name="other"></param>
let join (path: Path) (other: Path) =
    match (path, other) with
    | (Absolute path | Relative path), (Absolute other| Relative other) -> 
        System.IO.Path.Join (path, other)  |> Path.of_string

let relative_to (parent: Path) (child: Path) =
    match (parent, child) with
    | (Absolute parent | Relative parent), (Absolute child| Relative child) ->
        System.IO.Path.GetRelativePath (parent, child)
        |> Path.of_string

/// <summary>
/// Change the extension of the given path
/// </summary>
/// <param name="path"></param>
/// <param name="extension"></param>
let with_extension (path: Path) (extension: string) =
    path
    |> string_apply (fun path -> System.IO.Path.ChangeExtension (path, extension))
                  
/// <summary>
/// Change the basename of the path
/// </summary>
/// <param name="path"></param>
/// <param name="new_"></param>
let with_name (path: Path) (new_: string) =
    failwith "t"


let (|Nil|Cons|) (arr: 'a array) =
    match arr.Length with
    | 0 -> Nil
    | 1 -> Cons(arr.[0], [||])
    | _ -> Cons(arr.[0], arr.[1..])

/// <summary>
/// Determines wheter the path is normalized.
/// A normalized path is a path that does not contain any "." or ".." path segments and has no trailing or duplicate slashes
/// </summary>
/// <param name="path"></param>
let is_normalized (path_: Path) =

    let path: string = path_ |> function | Absolute a -> a | Relative a -> a

    let rec checkSegments = function
        | [] -> true
        | ("." | "..")::_ -> false
        | _::tail -> checkSegments tail

    not (path.Contains "//") && (String.split "/" path |> checkSegments)

// impure functions
// https://docs.python.org/3.8/library/pathlib.html

let resolve = function
    | Absolute path -> 
        Native.realpath path |> Result.map Path.of_string
    | Relative path -> 
        Native.realpath path |> Result.map Path.of_string
