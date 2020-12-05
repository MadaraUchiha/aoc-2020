open System
open System.Text.RegularExpressions

#load "Day5Input.fsx"
open Day5Input

type Seat =
    { row: int
      column: int }
    member this.id = (this.row * 8) + this.column

let seatOfId id =
    let (row, column) = Math.DivRem(id, 8)
    { row = row; column = column }


let boardingPassToId pass =
    pass
    |> fun p -> Regex.Replace(p, @"[BR]", "1")
    |> fun p -> Regex.Replace(p, @"[FL]", "0")
    |> fun p -> System.Convert.ToInt32(p, 2)

let boardingPassToSeat = boardingPassToId >> seatOfId

let parseInput (input: string) =
    input.Split('\n')
    |> List.ofArray
    |> List.map boardingPassToSeat

let day5Part1Solution =
    parseInput day5Input
    |> List.maxBy (fun seat -> seat.id)
    |> (fun seat -> seat.id)

let consecutivePresent i set =
    Set.contains (i + 1) set
    && Set.contains (i - 1) set

let day5Part2Solution =
    let knownSeatIds =
        parseInput day5Input
        |> Set.ofList
        |> Set.map (fun seat -> seat.id)

    let possibleSeatIds = [ 0 .. day5Part1Solution ] |> Set.ofList

    let missingSeatIds =
        Set.difference possibleSeatIds knownSeatIds

    let missingSetIdsWithPresentConsecutives =
        missingSeatIds
        |> Set.filter (fun id -> consecutivePresent id knownSeatIds)
        |> Set.toList

    match missingSetIdsWithPresentConsecutives with
    | [ only ] -> only
    | _ -> 0

printfn "Day 5 part 1 solution: %i" day5Part1Solution
printfn "Day 5 part 2 solution: %i" day5Part2Solution
