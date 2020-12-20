#load "../common/Utils.fsx"
#load "Day20Input.fsx"

open Utils
open Day20Input

type Tile = { ID: int; contents: char [,] }

type TileConfigurations =
    { ID: int
      configurations: char [,] list }

let rot90 (m: 'a [,]) =
    [ for col in (Array2D.length1 m) - 1 .. -1 .. 0 -> m.[0.., col] ]
    |> array2D

let rot180 = rot90 >> rot90
let rot270 = rot180 >> rot90

let flipHorizontal m =
    [ for row in (Array2D.length1 m) - 1 .. -1 .. 0 -> m.[row, 0..] ]
    |> array2D

let trimBorders (m: 'a [,]) =
    m.[1..(Array2D.length1 m - 2), 1..(Array2D.length1 m - 2)]

let assemble (nested: char [,] [,]): char [,] =
    let outerDim = Array2D.length1 nested
    let innerDim = Array2D.length1 nested.[0, 0]
    [ for outer in 0 .. outerDim - 1 do
        for inner in 0 .. innerDim - 1 ->
            nested.[outer, 0..]
            |> Seq.collect (fun m -> m.[inner, 0..])
            |> Seq.toList ]
    |> array2D

let topBorder (tile: 'a [,]) = tile.[0, 0..]
let leftBorder (tile: 'a [,]) = tile.[0.., 0]
let bottomBorder (tile: 'a [,]) = tile.[(Array2D.length1 tile - 1), 0..]
let rightBorder (tile: 'a [,]) = tile.[0.., (Array2D.length1 tile - 1)]

let getBorders (tile: 'a [,]) =
    [ topBorder tile
      bottomBorder tile
      leftBorder tile
      rightBorder tile ]

let configurations tile =
    [ tile
      tile |> rot90
      tile |> rot180
      tile |> rot270 ]
    |> List.collect (fun t -> [ t; flipHorizontal t ])

let findCorners configs tiles =
    let nbUnmatchedBorders tile (configs: TileConfigurations list) =
        let borders = getBorders tile.contents

        let nbMatchesPerBorder =
            borders
            |> Seq.map (fun border ->
                configs
                |> Seq.filter (fun config ->
                    config.configurations
                    |> List.exists (fun c -> c |> getBorders |> List.contains border))
                |> Seq.length)

        nbMatchesPerBorder
        |> Seq.filter ((=) 0)
        |> Seq.length

    tiles
    |> List.filter (fun t -> nbUnmatchedBorders t (configs |> List.filter (fun c -> c.ID <> t.ID)) = 2)

let configurationsTile (tile: Tile) =
    { ID = tile.ID
      configurations = configurations tile.contents }

let findPiecesThatMatch border (tiles: TileConfigurations seq): Tile seq =
    tiles
    |> Seq.choose (fun t ->
        t.configurations
        |> List.tryFind (fun c -> c |> getBorders |> Seq.contains border)
        |> Option.map (fun cfg -> { ID = t.ID; contents = cfg }))

let placeNextPiece (puzzle, configurations) (x, y) =
    let above =
        puzzle
        |> Map.tryFind (x, y - 1)
        |> Option.map (fun tile -> bottomBorder tile.contents)

    let left =
        puzzle
        |> Map.tryFind (x - 1, y)
        |> Option.map (fun tile -> rightBorder tile.contents)
    // find the piece that has above as upper border and left as left border
    let cfgsList =
        configurations |> Map.toSeq |> Seq.map snd

    let piece =
        match above, left with
        | None, None ->
            failwith
                "This can't be right, we always have the top left piece placed down so we always need an above and/or left!"
        | Some a, Some l ->
            cfgsList
            |> Seq.pick (fun tile ->
                tile.configurations
                |> Seq.tryFind (fun cfg -> a = topBorder cfg && l = leftBorder cfg)
                |> Option.map (fun cfg -> { ID = tile.ID; contents = cfg }))
        | Some a, None ->
            cfgsList
            |> Seq.pick (fun tile ->
                tile.configurations
                |> Seq.tryFind (fun cfg -> a = topBorder cfg)
                |> Option.map (fun cfg -> { ID = tile.ID; contents = cfg }))
        | None, Some l ->
            cfgsList
            |> Seq.pick (fun tile ->
                tile.configurations
                |> Seq.tryFind (fun cfg -> l = leftBorder cfg)
                |> Option.map (fun cfg -> { ID = tile.ID; contents = cfg }))

    (puzzle |> Map.add (x, y) piece, configurations |> Map.remove piece.ID)

let parseInput (input: string) =
    let parseTile str =
        match str |> split "\n" with
        | headline :: rest -> (sscanf "Tile %i:" headline), array2D rest
        | _ -> failwith "Invalid input"

    input
    |> trim
    |> split "\n\n"
    |> Seq.map
        (parseTile
         >> (fun (id, contents) -> { ID = id; contents = contents }))
    |> Seq.toList

let day20Part1Solution =
    let tiles = parseInput day20Input
    let configs = tiles |> List.map configurationsTile
    let corners = findCorners configs tiles

    corners
    |> Seq.map (fun t -> int64 t.ID)
    |> Seq.reduce (*)


let day20Part2Solution =
    let tiles = parseInput day20Input

    let configs =
        tiles
        |> List.map (fun t -> t.ID, configurationsTile t)
        |> Map.ofSeq

    let corners =
        findCorners (configs |> Map.toList |> List.map snd) tiles

    let topLeft = corners.[0]
    // how do we orient topleft? with a border that has matches down + right

    let topLeftConfigs = configs |> Map.find topLeft.ID
    let otherConfigs = configs |> Map.remove topLeft.ID

    let orientedTopLeft =
        topLeftConfigs.configurations
        |> List.find (fun cfg ->
            let right = cfg |> rightBorder
            let bottom = cfg |> bottomBorder
            (findPiecesThatMatch right (otherConfigs |> Map.toSeq |> Seq.map snd)
             |> Seq.length > 0)
            && (findPiecesThatMatch bottom (otherConfigs |> Map.toSeq |> Seq.map snd)
                |> Seq.length > 0))
        |> (fun cfg ->
            { ID = topLeftConfigs.ID
              contents = cfg })

    let size =
        sqrt (tiles |> Seq.length |> float) |> int

    // Start from top left
    let puzzle = [ (0, 0), orientedTopLeft ] |> Map.ofSeq

    let tasks =
        [ for y in 0 .. (size - 1) do
            for x in 0 .. (size - 1) -> (x, y) ]
        |> List.except [ (0, 0) ]

    let (completedPuzzle, _) =
        tasks
        |> List.fold placeNextPiece (puzzle, otherConfigs)
    // Let's trim everything and dump in a single bitmap
    let trimmed =
        [ for col in 0 .. size - 1 do
            [ for row in 0 .. size - 1 ->
                let trimmedPiece =
                    completedPuzzle
                    |> Map.find (row, col)
                    |> (fun tile -> trimBorders tile.contents)

                trimmedPiece ] ]
        |> array2D

    let assembled = assemble trimmed

    let parsedMonster =
        [ for (y, row) in monsterPattern |> split "\n" |> Seq.indexed do
            for (x, char) in row |> Seq.indexed do
                if char = '#' then yield (x, y) ]

    let pictureSize = assembled |> Array2D.length1 // still a square

    let locations =
        [ for y in 0 .. pictureSize - 1 do
            for x in 0 .. pictureSize - 1 -> (x, y) ]

    let wasMonsterFound (grid: char [,]) (x, y) =
        let size = grid |> Array2D.length1

        parsedMonster
        |> Seq.forall (fun (mx, my) ->
            x
            + mx < size
            && y + my < size
            && grid.[x + mx, y + my] = '#')

    let hitsPerConfigurations =
        assembled
        |> configurations
        |> List.map (fun merged ->
            locations
            |> List.filter (fun loc -> wasMonsterFound merged loc))

    let monsterLocations =
        hitsPerConfigurations
        |> List.sortByDescending Seq.length
        |> Seq.head

    let totalMonstersFound = monsterLocations |> Seq.length
    let monsterSize = parsedMonster |> Seq.length

    let totalRoughSpots =
        [ for row in 0 .. pictureSize - 1 do
            for col in 0 .. pictureSize - 1 do
                if assembled.[row, col] = '#' then yield 1 ]
        |> List.sum

    totalRoughSpots
    - (monsterSize * totalMonstersFound)

printfn "Day 20 part 1 solution: %i" day20Part1Solution
printfn "Day 20 part 2 solution: %i" day20Part2Solution
