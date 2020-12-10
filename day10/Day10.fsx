#load "Day10Input.fsx"
#load "../common/Utils.fsx"

open Day10Input
open Utils

let toDifferences ns =
    ns
    |> Seq.pairwise
    |> Seq.map (fun (a, b) -> b - a)

let countPaths ns =
    let mutable cache = Map.empty

    let indexed = ns |> List.indexed

    let rec folder from =
        match cache |> Map.tryFind from with
        | Some n -> n
        | None ->
            match indexed.[from..] with
            | [] -> 0L // Will never be reached. F# ranges are inclusive.
            | (_, curr) :: tail ->
                match tail with
                | [] -> 1L
                | tail ->
                    tail
                    |> List.filter (fun (_, next) -> next - curr <= 3)
                    |> List.sumBy (fun (idx, _) ->
                        let res = folder idx
                        cache <- Map.add idx res cache
                        res)

    folder 0

let parseInput input = input |> split "\n" |> List.map int

let day10Part1Solution =
    let jolts = parseInput day10Input |> Seq.sort

    let differences =
        seq {
            yield 0
            yield! jolts
            yield (Seq.max jolts) + 3
        }
        |> toDifferences
        |> Seq.countBy id
        |> Map.ofSeq

    differences.[1] * differences.[3]

let day10Part2Solution =
    parseInput day10Input
    |> List.sort
    |> (fun list -> 0 :: list @ [ List.max list + 3 ])
    |> countPaths

printfn "Day 10 part 1 solution: %i" day10Part1Solution
printfn "Day 10 part 2 solution: %i" day10Part2Solution
