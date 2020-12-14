#load "Day12Input.fsx"
#load "../common/Utils.fsx"

open Day12Input
open Utils

type Action =
    | N of int
    | S of int
    | E of int
    | W of int
    | L of int
    | R of int
    | F of int

type Direction =
    | East = 0
    | North = 1
    | West = 2
    | South = 3


type Ship =
    { Direction: Direction
      X: int
      Y: int
      Waypoint: int * int }

let vector direction =
    match direction with
    | Direction.East -> (1, 0)
    | Direction.North -> (0, 1)
    | Direction.West -> (-1, 0)
    | Direction.South -> (0, -1)
    | n -> failwith (sprintf "Will never arrive: %A" n)

let fakeSin n =
    match n %% 360 with
    | 0 -> 0
    | 90 -> 1
    | 180 -> 0
    | 270 -> -1
    | _ -> failwith "Invalid fakeSin"

let fakeCos n =
    match n %% 360 with
    | 0 -> 1
    | 90 -> 0
    | 180 -> -1
    | 270 -> 0
    | _ -> failwith "Invalid fakeCos"

let rotateVector angle vector =
    let (x, y) = vector
    (fakeCos angle * x - fakeSin angle * y, fakeSin angle * x + fakeCos angle * y)

let rotateWaypoint action waypoint =
    match action with
    | L n -> rotateVector n waypoint
    | R n -> rotateVector -n waypoint
    | _ -> failwith "Will never arrive"


let turn (direction: Direction) action =
    let intDirection = int direction
    let offset n = n / 90

    match action with
    | L n -> enum<Direction> ((intDirection + offset n) %% 4)
    | R n -> enum<Direction> ((intDirection - offset n) %% 4)
    | _ -> failwith "Will never arrive"

let takeStep ship action =
    match action with
    | L n ->
        { ship with
              Direction = turn ship.Direction (L n) }
    | R n ->
        { ship with
              Direction = turn ship.Direction (R n) }
    | N n -> { ship with Y = ship.Y + n }
    | S n -> { ship with Y = ship.Y - n }
    | E n -> { ship with X = ship.X + n }
    | W n -> { ship with X = ship.X - n }
    | F n ->
        let x, y = vector ship.Direction
        { ship with
              Y = ship.Y + (y * n)
              X = ship.X + (x * n) }

let takeRealStep ship action =
    match action with
    | L n ->
        { ship with
              Waypoint = rotateWaypoint (L n) ship.Waypoint }
    | R n ->
        { ship with
              Waypoint = rotateWaypoint (R n) ship.Waypoint }
    | N n ->
        { ship with
              Waypoint = (fst ship.Waypoint, snd ship.Waypoint + n) }
    | S n ->
        { ship with
              Waypoint = (fst ship.Waypoint, snd ship.Waypoint - n) }
    | E n ->
        { ship with
              Waypoint = (fst ship.Waypoint + n, snd ship.Waypoint) }
    | W n ->
        { ship with
              Waypoint = (fst ship.Waypoint - n, snd ship.Waypoint) }
    | F n ->
        { ship with
              Y = ship.Y + (n * snd ship.Waypoint)
              X = ship.X + (n * fst ship.Waypoint) }

let toAction (str: string) =
    let char = str.[0]
    let rest = int str.[1..]
    match char with
    | 'E' -> E rest
    | 'N' -> N rest
    | 'W' -> W rest
    | 'S' -> S rest
    | 'L' -> L rest
    | 'R' -> R rest
    | 'F' -> F rest
    | _ -> failwith "Invalid input."

let parseInput input = input |> split "\n" |> Seq.map toAction

let initialShip =
    { Direction = Direction.East
      X = 0
      Y = 0
      Waypoint = (10, 1) }

let day12Part1Solution =
    parseInput day12Input
    |> Seq.fold takeStep initialShip
    |> (fun { X = x; Y = y } -> abs x + abs y)

let day12Part2Solution =
    parseInput day12Input
    |> Seq.fold takeRealStep initialShip
    |> (fun { X = x; Y = y } -> abs x + abs y)

printfn "Day 12 part 1 solution: %i" day12Part1Solution
printfn "Day 12 part 2 solution: %i" day12Part2Solution
