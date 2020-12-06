open System
open System.Text.RegularExpressions

let splitMany (substrs: string seq) (str: string) =
    str.Split(substrs |> Array.ofSeq, StringSplitOptions.None)
    |> List.ofArray

let split substr str = splitMany [ substr ] str

module List =
    let filteri predicate values =
        values
        |> List.indexed
        |> List.filter (fun (i, value) -> predicate i value)
        |> List.map snd

module RegExp =
    let replace (pattern: string) (replacement: string) (source: string) =
        Regex.Replace(source, pattern, replacement)
