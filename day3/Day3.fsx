#load "Day3Input.fsx"
open Day3Input

// Utility
module Array =
    let filteri predicate values =
        values
        |> Array.indexed
        |> Array.filter (fun (i, value) -> predicate i value)
        |> Array.map snd

module Tile =
    type Tile =
        | Tree
        | Snow

    let private tileFromChar c =
        match c with
        | '#' -> Tree
        | _ -> Snow

    let rowFromString s = Array.ofSeq s |> Array.map tileFromChar

let parseInput (input: string) =
    input.Split([| '\n' |])
    |> Array.map ((fun r -> r.Trim()) >> Tile.rowFromString)

type Vector = { x: int; y: int }

let tileAtIndex<'T> (row: 'T []) index = row.[index % Array.length row]


let solveFor { x = advanceX; y = advanceY } tiles =
    tiles
    |> Array.filteri (fun i _ -> i % advanceY = 0)
    |> Array.mapi (fun i row -> tileAtIndex row (i * advanceX))
    |> Array.filter (fun t -> t = Tile.Tree)
    |> Array.length

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
