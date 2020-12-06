#load "Day3Input.fsx"
#load "../common/Utils.fsx"
open Day3Input
open Utils

module Tile =
    type Tile =
        | Tree
        | Snow

    let private tileFromChar c =
        match c with
        | '#' -> Tree
        | _ -> Snow

    let rowFromString s = Array.ofSeq s |> Array.map tileFromChar

let parseInput input =
    input |> split "\n" |> List.map Tile.rowFromString

type Vector = { x: int; y: int }

let tileAtIndex<'T> (row: 'T []) index = row.[index % Array.length row]


let solveFor { x = advanceX; y = advanceY } tiles =
    tiles
    |> List.filteri (fun i _ -> i % advanceY = 0)
    |> List.mapi (fun i row -> tileAtIndex row (i * advanceX))
    |> List.filter (fun t -> t = Tile.Tree)
    |> List.length

let tiles = parseInput day3Input

let day3Part1Solution = tiles |> solveFor { x = 3; y = 1 }

let day3Part2Solution =
    let slopes =
        [ { x = 1; y = 1 }
          { x = 3; y = 1 }
          { x = 5; y = 1 }
          { x = 7; y = 1 }
          { x = 1; y = 2 } ]

    slopes
    |> List.map ((fun slope -> solveFor slope tiles) >> int64)
    |> List.fold (*) (int64 1)

printfn "Day 3 part 1 solution: %i" day3Part1Solution
printfn "Day 3 part 2 solution: %i" day3Part2Solution
