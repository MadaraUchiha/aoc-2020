#load "../../../common/Utils.fsx"
#load "Day10Input.fsx"

open Utils
open Day10Input
open System

type Vector = double * double

let toDeg (rad: double) = rad / Math.PI * 180.0

type Projection =
    | Positive of mean: double
    | Negative of mean: double

let calculateProjection (x0, y0) (x, y) =
    let mean = (y0 - y) / (x0 - x)

    let meanfinity = // normalize infinity. Fuck you floating points.
        if Double.IsInfinity mean then -infinity else mean

    if x = x0
    then if y0 > y then Positive meanfinity else Negative meanfinity
    else if x0 < x
    then Positive meanfinity
    else Negative meanfinity

let calculateAllProjections asteroids =
    asteroids
    |> Seq.map (fun asteroid ->
        asteroid,
        asteroids
        |> Seq.filter ((<>) asteroid)
        |> Seq.map (fun otherAsteroid -> otherAsteroid, calculateProjection asteroid otherAsteroid))

let parseInput input =
    input
    |> split "\n"
    |> Seq.indexed
    |> Seq.collect (fun (y, row) ->
        row
        |> Seq.indexed
        |> Seq.filter (snd >> (=) '#')
        |> Seq.map (fun (x, _) -> double x, double y))
    |> Set.ofSeq

let day10Part1Solution =
    let asteroids = parseInput day10Input

    asteroids
    |> calculateAllProjections
    |> Seq.mapSnd Set
    |> Seq.maxBy (snd >> (Set.map snd) >> Set.count)
    |> (snd >> (Set.map snd) >> Set.count)


let proximityTo (x0, y0) (x, y) = abs (y - y0 + x - x0)

let spreadColumns lists =
    let rec fold acc lists =
        match lists with
        | [] -> acc
        | lists ->
            let newAcc =
                acc
                @ (lists |> List.map List.tryHead |> List.choose id)

            let newLists =
                lists
                |> List.filter (List.length >> (<>) 1)
                |> List.map List.tail

            fold newAcc newLists

    fold List.empty lists

let compareFloats a b =
    if a > b then 1
    else if a < b then -1
    else 0

let day10Part2Solution =
    let asteroids = parseInput day10Input

    let origin, projections =
        asteroids
        |> calculateAllProjections
        |> Seq.maxBy (snd >> Set >> Set.map snd >> Set.count)

    let lines =
        projections
        |> Seq.groupBy snd
        |> Seq.mapSnd (Seq.map fst >> Seq.toList)
        |> Seq.sortWith (fun (a, _) (b, _) ->
            match a, b with
            | Positive _, Negative _ -> -1
            | Negative _, Positive _ -> 1
            | Positive a, Positive b -> compareFloats a b
            | Negative a, Negative b -> compareFloats a b)


    lines
    |> Seq.mapSnd (List.sortBy (proximityTo origin))
    |> Seq.map snd
    |> Seq.toList

    |> spreadColumns
    |> List.item 199 // 200th, index 0
    |> (fun (x, y) -> x * 100.0 + y)

printfn "2019 Day 10 part 1 solution: %i" day10Part1Solution
printfn "2019 Day 10 part 2 solution: %i" (int day10Part2Solution)
