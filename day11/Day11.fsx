#load "Day11Input.fsx"
#load "../common/Utils.fsx"

open Day11Input
open Utils

type TileKind =
    | Floor
    | Seat

type Tile =
    { Tile: TileKind
      X: int
      Y: int
      Occupied: bool }

type GameMap = Map<int * int, Tile>

let isOccupied t = t.Occupied
let isSeat t = t.Tile = Seat

let tileFromChar c =
    match c with
    | '.' -> Floor
    | 'L' -> Seat
    | _ -> failwith "Invalid input."

let directions =
    [ -1 .. 1 ]
    |> Seq.collect (fun x -> [ -1 .. 1 ] |> Seq.map (fun y -> (x, y)))
    |> Seq.filter (fun v -> v <> (0, 0))

let rec toFirstFound fn (x, y) tile (map: GameMap) =
    let foundTile = Map.tryFind (x + tile.X, y + tile.Y) map
    match foundTile with
    | None -> None
    | Some t ->
        match fn t with
        | true -> Some t
        | false -> toFirstFound fn (x, y) t map

let findInLineOfSight predicate tile map =
    directions
    |> Seq.map (fun vector -> toFirstFound predicate vector tile map)
    |> Seq.filter Option.isSome
    |> Seq.map Option.get

let findAdjacent = findInLineOfSight (fun _ -> true)

let findProjected = findInLineOfSight isSeat

let visitTile seatTolerance tileFinder tile map =
    let occupiedAdjacent =
        map
        |> tileFinder tile
        |> Seq.filter isOccupied
        |> Seq.length

    match tile.Tile, tile.Occupied, occupiedAdjacent with
    | Seat, false, adj when adj = 0 -> { tile with Occupied = true }
    | Seat, true, adj when adj >= seatTolerance -> { tile with Occupied = false }
    | _ -> tile

let playOneGeneration seatTolerance tileFinder map =
    map
    |> Map.map (fun _ tile -> visitTile seatTolerance tileFinder tile map)

let rec playToEquilibrium seatTolerance tileFinder tiles =
    let curr =
        playOneGeneration seatTolerance tileFinder tiles

    if curr = tiles
    then tiles
    else playToEquilibrium seatTolerance tileFinder curr

let parseInput input =
    input
    |> split "\n"
    |> Seq.indexed
    |> Seq.collect (fun (y, row) ->
        row
        |> (Seq.toList
            >> Seq.map tileFromChar
            >> Seq.indexed
            >> Seq.map (fun (x, tile) ->
                (x, y),
                { Tile = tile
                  X = x
                  Y = y
                  Occupied = false })))
    |> Map.ofSeq

let day11Part1Solution =
    parseInput day11Input
    |> playToEquilibrium 4 findAdjacent
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.filter isOccupied
    |> Seq.length

let day11Part2Solution =
    parseInput day11Input
    |> playToEquilibrium 5 findProjected
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.filter isOccupied
    |> Seq.length

printfn "Day 11 part 1 solution: %i" day11Part1Solution
printfn "Day 11 part 2 solution: %i" day11Part2Solution
