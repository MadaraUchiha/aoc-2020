#load "Day8Input.fsx"
#load "../common/Utils.fsx"
open Day8Input
open Utils

module Instruction =
    type T =
        | Nop of int
        | Acc of int
        | Jmp of int
        | Term

    let ofString str =
        let pair = str |> split " "

        match pair with
        | [ "nop"; n ] -> Nop(int n)
        | [ "acc"; n ] -> Acc(int n)
        | [ "jmp"; n ] -> Jmp(int n)
        | _ -> failwith "Invalid input"

type Result =
    | Looped of int
    | Finished of int

let runToCompletionOrFirstLoop instructions =
    let rec run currPtr found acc (list: Instruction.T list) =
        let instruction = list.[currPtr]
        if Set.contains currPtr found then
            Looped acc
        else
            match instruction with
            | Instruction.Jmp n -> run (currPtr + n) (Set.add currPtr found) acc list
            | Instruction.Nop _ -> run (currPtr + 1) (Set.add currPtr found) acc list
            | Instruction.Acc n -> run (currPtr + 1) (Set.add currPtr found) (acc + n) list
            | Instruction.Term -> Finished acc

    instructions
    @ [ Instruction.Term ]
    |> run 0 Set.empty 0

let parseInput input =
    input
    |> split "\n"
    |> List.map Instruction.ofString

let day8Part1Solution =
    parseInput day8Input |> runToCompletionOrFirstLoop

let updateElement f key st =
    st
    |> List.map (function
        | (k, v) when k = key -> k, f v
        | e -> e)

let switcheroo instruction =
    match instruction with
    | Instruction.Jmp n -> Instruction.Nop n
    | Instruction.Nop n -> Instruction.Jmp n
    | i -> i

let day8Part2Solution =
    let instructionSet = parseInput day8Input
    instructionSet
    |> List.map (fun _ -> List.indexed instructionSet)
    |> List.indexed
    |> Seq.ofList // lazy
    |> Seq.filter (fun (i, list) ->
        match list.[i] with
        | _, Instruction.Nop _
        | _, Instruction.Jmp _ -> true
        | _ -> false)
    |> Seq.map
        ((fun entry -> entry ||> updateElement switcheroo)
         >> List.map snd
         >> runToCompletionOrFirstLoop)
    |> Seq.find (function
        | Finished _ -> true
        | _ -> false)

printfn "Day 8 part 1 solution: %A" day8Part1Solution
printfn "Day 8 part 2 solution: %A" day8Part2Solution
