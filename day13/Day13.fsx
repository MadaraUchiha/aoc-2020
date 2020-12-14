#load "Day13Input.fsx"
#load "ChineseRemainderTheorem.fsx"
#load "../common/Utils.fsx"

open Day13Input
open Utils
open ChineseRemainderTheorem

type BusSlot =
    | Id of int
    | X

let toOption =
    function
    | Id n -> Some n
    | X -> None

let findClosestBus timestamp schedule =
    schedule
    |> List.choose toOption
    |> List.map (fun n -> n, (((timestamp / n) + 1) * n) - timestamp)
    |> List.minBy (snd)

let parseInput input =
    match input |> split "\n" with
    | [ myTimestamp; busSchedule ] ->
        (int myTimestamp,
         busSchedule
         |> split ","
         |> List.map (function
             | "x" -> X
             | id -> Id(int id)))
    | _ -> failwith "Invalid input"

let day13Part1Solution =
    parseInput day13Input ||> findClosestBus ||> (*)

let day13Part2Solution =
    parseInput day13Input
    |> snd
    |> List.indexed
    |> List.choose (function
        | i, Id x -> Some((int64 -i) %% (int64 x), int64 x)
        | _ -> None)
    |> chineseRemainderTheorem

printfn "Day 13 part 1 solution: %i" day13Part1Solution
printfn "Day 13 part 2 solution: %i" day13Part2Solution
