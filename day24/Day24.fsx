#load "../common/Utils.fsx"
#load "Day24Input.fsx"

open Utils
open Day24Input

type Instruction =
    | E
    | W
    | SE
    | SW
    | NE
    | NW
    static member toCoordinate ins =
        match ins with
        | E -> (1, -1, 0)
        | W -> (-1, 1, 0)
        | SE -> (0, -1, 1)
        | SW -> (-1, 0, 1)
        | NE -> (1, 0, -1)
        | NW -> (0, 1, -1)

let addVectors (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

let flipTiles tiles =
    let rec flipTiles' acc remaining =
        match remaining with
        | [] -> acc
        | head :: tail ->
            let black = Map.findOr false head acc
            let newAcc = Map.add head (not black) acc

            flipTiles' newAcc tail

    flipTiles' Map.empty tiles

let neighborsOf tile =
    [ E; W; SE; SW; NE; NW ]
    |> List.map (Instruction.toCoordinate >> addVectors tile)

let withNeighbors map =
    map
    |> Map.toSeq
    |> Seq.filter (snd) // Consider only neighbors of flipped tiles.
    |> Seq.map fst
    |> Seq.fold
        (fun acc tile ->
            neighborsOf tile
            |> List.fold (fun miniAcc t -> Map.addUnlessExists t false miniAcc) acc)
        map

let playGameOfLife tilesMap =
    let biggerMap = withNeighbors tilesMap

    biggerMap
    |> Map.fold
        (fun acc tile isBlack ->
            let blackNeighbors =
                neighborsOf tile
                |> List.filter (fun neighbor -> Map.findOr false neighbor biggerMap)
                |> List.length

            match isBlack, blackNeighbors with
            | true, n when n = 0 || n > 2 -> Map.add tile false acc
            | false, 2 -> Map.add tile true acc
            | _ -> acc)
        biggerMap
    |> Map.filter (fun _ black -> black)

let rec playGameOfLifeN n tilesMap =
    if n = 0
    then tilesMap
    else playGameOfLifeN (n - 1) (playGameOfLife tilesMap)

let parseInstruction instruction =
    let rec parse' acc remaining =
        match remaining with
        | "" -> acc
        | Scan "e%s" rest -> parse' (acc @ [ E ]) rest
        | Scan "w%s" rest -> parse' (acc @ [ W ]) rest
        | Scan "se%s" rest -> parse' (acc @ [ SE ]) rest
        | Scan "sw%s" rest -> parse' (acc @ [ SW ]) rest
        | Scan "ne%s" rest -> parse' (acc @ [ NE ]) rest
        | Scan "nw%s" rest -> parse' (acc @ [ NW ]) rest
        | otherwise -> failwithf "Invalid input: %s" otherwise

    parse' List.empty instruction

let parseInput = split "\n" >> List.map parseInstruction

let day24Part1Solution =
    day24Input
    |> parseInput
    |> List.map (
        List.map Instruction.toCoordinate
        >> List.reduce addVectors
    )
    |> flipTiles
    |> Map.filter (fun _ black -> black)
    |> Map.count

let day24Part2Solution =
    day24Input
    |> parseInput
    |> List.map (
        List.map Instruction.toCoordinate
        >> List.reduce addVectors
    )
    |> flipTiles
    |> Map.filter (fun _ black -> black)
    |> playGameOfLifeN 100
    |> Map.count

printfn "Day 24 part 1 solution: %i" day24Part1Solution
printfn "Day 24 part 2 solution: %i" day24Part2Solution
