open System
open System.Text.RegularExpressions

#load "Day5Input.fsx"
#load "../common/Utils.fsx"
open Day5Input
open Utils

type Seat =
    { row: int
      column: int }
    member this.id = (this.row * 8) + this.column


let idOfSeat (seat: Seat) = seat.id

let seatOfCoords (row, column) = { row = row; column = column }

let seatOfId id = Math.DivRem(id, 8) |> seatOfCoords

let boardingPassToId pass =
    pass
    |> RegExp.replace @"[BR]" "1"
    |> RegExp.replace @"[FL]" "0"
    |> fun p -> System.Convert.ToInt32(p, 2)

let boardingPassToSeat = boardingPassToId >> seatOfId

let parseInput input =
    input |> split "\n" |> List.map boardingPassToSeat

let day5Part1Solution =
    parseInput day5Input
    |> List.maxBy idOfSeat
    |> idOfSeat

let consecutivePresent set i =
    Set.contains (i + 1) set
    && Set.contains (i - 1) set

let day5Part2Solution =
    let knownSeatIds =
        parseInput day5Input
        |> Set.ofList
        |> Set.map idOfSeat

    let possibleSeatIds = [ 0 .. day5Part1Solution ] |> Set.ofList

    let missingSeatIds =
        Set.difference possibleSeatIds knownSeatIds

    let missingSetIdsWithPresentConsecutives =
        missingSeatIds
        |> Set.filter (consecutivePresent knownSeatIds)
        |> Set.toList

    match missingSetIdsWithPresentConsecutives with
    | [ only ] -> only
    | _ -> 0

printfn "Day 5 part 1 solution: %i" day5Part1Solution
printfn "Day 5 part 2 solution: %i" day5Part2Solution
