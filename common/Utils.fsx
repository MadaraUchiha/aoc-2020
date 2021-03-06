#r "nuget: FSharpPlus"

open System
open System.Text.RegularExpressions
open FSharpPlus


let splitMany (substrs: string seq) (str: string) =
    str.Split(substrs |> Array.ofSeq, StringSplitOptions.None)
    |> List.ofArray

let split substr str = splitMany [ substr ] str

let trimEnd (c: char) (str: string) = str.TrimEnd(c)
let trim (str: string) = str.Trim()

let charToInt (c: char) = int c - int '0' // lol

module List =
    let filteri predicate values =
        values
        |> List.indexed
        |> List.filter (fun (i, value) -> predicate i value)
        |> List.map snd

module Seq =
    let mapFst fn = Seq.map (fun (a, b) -> (fn a, b))
    let mapSnd fn = Seq.map (fun (a, b) -> (a, fn b))
    let mapBoth fn = Seq.map (fun (a, b) -> (fn a, fn b))

module Map =
    let findOr defaultValue key map =
        match Map.tryFind key map with
        | Some x -> x
        | None -> defaultValue

    let mapKeys fn map =
        map
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> (fn k, v))
        |> Map.ofSeq

    let addUnlessExists key value map =
        if Map.containsKey key map then map else Map.add key value map

module RegExp =
    let replace (pattern: string) (replacement: string) (source: string) =
        Regex.Replace(source, pattern, replacement)

    let (|Pattern|_|) pattern input =
        let m = Regex.Match(input, pattern)

        if m.Success
        then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

let inline (|Scan|_|) pattern input = trySscanf pattern input

let inline (%%) x m =
    let mod' = x % m
    if sign mod' >= 0 then mod' else abs (mod' + m)

// Expose outwards
let inline trySscanf pattern = trySscanf pattern
let inline sscanf pattern = sscanf pattern
