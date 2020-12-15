#load "../common/Utils.fsx"

open Utils

type GameState =
    { InitialQueue: int list
      Memory: Map<int, int>
      Turn: int * int }

let flipTuple (a, b) = b, a

let indicdesMap ns = ns |> List.map flipTuple |> Map.ofSeq

let initGame ns lastN =
    { Memory = indicdesMap ns
      Turn = lastN
      InitialQueue = ns |> List.map snd }

let playStep state =
    let turnNumber, spokenNumber = state.Turn

    let newMemory =
        Map.add spokenNumber turnNumber state.Memory

    match Map.tryFind spokenNumber state.Memory with
    | None ->
        { state with
              Memory = newMemory
              Turn = turnNumber + 1, 0 }
    | Some n ->
        { state with
              Memory = newMemory
              Turn = turnNumber + 1, turnNumber - n }

let playForever state =
    seq {
        yield! state.InitialQueue
        yield!
            Seq.unfold (fun state ->
                let newState = playStep state
                Some(snd state.Turn, newState)) { state with InitialQueue = List.empty }
    }

let day15Input = "9,12,1,4,17,0,18"

let parseInput input =
    let revNumList =
        input
        |> split ","
        |> List.map int
        |> List.indexed
        |> List.rev

    match revNumList with
    | []
    | [ _ ] -> failwith "Invalid input"
    | last :: rest -> (List.rev rest, last) ||> initGame

let playTill n game =
    let stopwatch = System.Diagnostics.Stopwatch.StartNew()

    let res =
        playForever game |> Seq.take n |> Seq.last

    stopwatch.Stop()
    printfn "Playing %i steps took %ims" n stopwatch.ElapsedMilliseconds
    res

let day15Part1Solution = parseInput day15Input |> playTill 2020

let day15Part2Solution =
    parseInput day15Input |> playTill 30000000

printfn "This may take a while (around 100 seconds)..."

printfn "Day 15 part 1 solution: %i" day15Part1Solution
printfn "Day 15 part 2 solution: %i" day15Part2Solution
