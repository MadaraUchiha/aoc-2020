open System

#load "Day6Input.fsx"
open Day6Input

#load "../common/utils.fsx"
open Utils

let parseInput input =
    input
    |> split "\n\n"
    |> List.map (split "\n" >> List.map Set.ofSeq)

let day6Part1Solution =
    parseInput day6Input
    |> List.sumBy (Set.unionMany >> Set.count)

let day6Part2Solution =
    parseInput day6Input
    |> List.sumBy (Set.intersectMany >> Set.count)

printfn "Day 6 part 1 solution: %i" day6Part1Solution
printfn "Day 6 part 2 solution: %i" day6Part2Solution
