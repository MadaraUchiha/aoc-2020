#load "Day1Input.fsx"
#load "../common/Utils.fsx"
open Day1Input
open Utils

let findPairSummingTo n set =
    set
    |> Seq.tryFind (fun a -> set |> Set.contains (n - a))
    |> Option.map (fun a -> (a, n - a))

let findTripletsSummingTo n set =
    set
    |> Seq.map (fun a -> (a, findPairSummingTo (n - a) set))
    |> Seq.tryFind (snd >> Option.isSome)
    |> Option.map (function
        | a, Some (b, c) -> Some(a, b, c)
        | _ -> None)
    |> Option.flatten

let parseInput input =
    input |> split "\n" |> Set.ofList |> Set.map int

let day1Part1Solution =
    parseInput day1Input |> findPairSummingTo 2020

let day1Part2Solution =
    parseInput day1Input |> findTripletsSummingTo 2020

match day1Part1Solution with
| Some (a, b) -> printfn "%i + %i = %i, %i * %i = %i" a b (a + b) a b (a * b)
| None -> do printfn "Part1: Not found."

match day1Part2Solution with
| Some (a, b, c) -> printfn "%i + %i + %i = %i, %i * %i * %i = %i" a b c (a + b + c) a b c (a * b * c)
| None -> do printfn "Part2: Not found."
