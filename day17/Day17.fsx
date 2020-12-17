#load "../common/Utils.fsx"

open Utils

let day17Input = "#...#.#.
..#.#.##
..#..#..
.....###
...#.#.#
#.#.##..
#####...
.#.#.##."

type CubeState =
    | Active
    | Inactive

let generateGrid minCoords maxCoords =
    let minMax = List.zip minCoords maxCoords
    let init = minMax |> List.map (fun _ -> [])

    minMax
    |> List.fold (fun acc (min, max) ->
        [ min - 1 .. max + 1 ]
        |> List.collect (fun n ->
            acc
            |> List.map (fun formingCoords -> formingCoords @ [ n ]))) init
    |> Set.ofList
    |> Set.toList


let generateAdjacent n =
    let zeroCoordinate = Array.zeroCreate n |> Array.toList

    generateGrid zeroCoordinate zeroCoordinate
    |> Seq.filter ((<>) zeroCoordinate)
    |> Seq.toList

let findEdges (coords: int list list) =
    let maxOfCol (col: int) (list: int list list) =
        list
        |> List.map (fun item -> item.[col])
        |> List.max

    let minOfCol (col: int) (list: int list list) =
        list
        |> List.map (fun item -> item.[col])
        |> List.min

    let coordsSize = coords.[0] |> List.length

    [ 0 .. coordsSize - 1 ]
    |> List.map (fun col -> (minOfCol col coords, maxOfCol col coords))
    |> List.unzip

let findAdjacent predicate state (origin: int list) =
    generateAdjacent (List.length origin)
    |> Seq.map (fun gridCoords -> List.map2 (+) gridCoords origin)
    |> Seq.filter (fun coords -> predicate (Map.findOr Inactive coords state))

let findActiveAdjacent = findAdjacent ((=) Active)

let play initialState =
    let playOneStep (state: Map<int list, CubeState>) =
        let (minCoords, maxCoords) =
            state
            |> Map.toSeq
            |> Seq.filter (snd >> (=) Active)
            |> Seq.map fst
            |> Seq.toList
            |> findEdges

        let fullGrid = generateGrid minCoords maxCoords

        let newState =
            fullGrid
            |> Seq.map (fun coords ->
                let activeAround =
                    (findActiveAdjacent state coords) |> Seq.length

                match Map.findOr Inactive coords state, activeAround with
                | Active, 2
                | Active, 3
                | Inactive, 3 -> coords, Active
                | _ -> coords, Inactive)
            |> Map.ofSeq


        Some(newState, newState)

    Seq.unfold playOneStep initialState

let parseChar =
    function
    | '.' -> Inactive
    | '#' -> Active
    | c -> failwithf "Invalid character %c" c

let parseInput n input =
    let padding = Array.zeroCreate (n - 2) |> Array.toList
    input
    |> split "\n"
    |> Seq.map
        (Array.ofSeq
         >> Seq.ofArray
         >> Seq.map parseChar
         >> Seq.indexed)
    |> Seq.indexed
    |> Seq.collect (fun (y, row) -> Seq.map (fun (x, c) -> ([ x; y ] @ padding, c)) row)
    |> Map.ofSeq

let day17Part1Solution =
    day17Input
    |> parseInput 3
    |> play
    |> Seq.item 5 // item index 5 = 6 steps played
    |> Map.filter (fun _ cube -> cube = Active)
    |> Map.count

let day17Part2Solution =
    day17Input
    |> parseInput 4
    |> play
    |> Seq.item 5 // item index 5 = 6 steps played
    |> Map.filter (fun _ cube -> cube = Active)
    |> Map.count

printfn "Day 17 part 1 solution: %i" day17Part1Solution
printfn "Day 17 part 2 solution: %i" day17Part2Solution
