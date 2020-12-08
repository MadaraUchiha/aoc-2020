open System
open System.Text.RegularExpressions

let splitMany (substrs: string seq) (str: string) =
    str.Split(substrs |> Array.ofSeq, StringSplitOptions.None)
    |> List.ofArray

let split substr str = splitMany [ substr ] str

let trimEnd (c: char) (str: string) = str.TrimEnd(c)
let trim (str: string) = str.Trim()

module List =
    let filteri predicate values =
        values
        |> List.indexed
        |> List.filter (fun (i, value) -> predicate i value)
        |> List.map snd

module RegExp =
    let replace (pattern: string) (replacement: string) (source: string) =
        Regex.Replace(source, pattern, replacement)

    let (|Pattern|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success
        then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None
