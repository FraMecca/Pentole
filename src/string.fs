module Pentole.String

let split(separator: string) (target: string) : string list =
    if System.String.IsNullOrEmpty separator  then
        [target]
    else
        let slen = separator.Length
        let rec split (acc: string list) (start_idx: int) =
            match target.IndexOf(separator, start_idx) with
            | -1 -> 
                if start_idx < target.Length then
                    target.Substring(start_idx) :: acc 
                else
                    acc 
            | index ->
                let part = target.Substring (start_idx, index - start_idx)
                split (part :: acc) (index + slen) 
        split [] 0
