#load "Day14Input.fsx"
#load "../common/Utils.fsx"

open Day14Input
open Utils

type BitType =
    | Zero
    | One
    | X

type Instruction =
    | Mask of BitType list
    | Op of int64 * int64 // address * value

type Computer =
    { Memory: Map<int64, int64>
      Mask: BitType list }

let charToInt (c: char) = int c - int '0' // lol

let Mask (maskStr: string) =
    maskStr.ToCharArray()
    |> Array.rev
    |> List.ofArray
    |> List.map (function
        | '0' -> Zero
        | '1' -> One
        | 'X' -> X
        | c -> failwithf "Invalid character %c" c)
    |> Mask

let Op address value = Op(address, value)

let rec applyMask mask value =
    mask
    |> List.indexed
    |> List.choose (function
        | i, One -> Some(i, 1)
        | i, Zero -> Some(i, 0)
        | _ -> None)
    |> Seq.fold (fun res (pos, bit) ->
        match bit with
        | 0 -> res &&& ~~~(1L <<< pos)
        | 1 -> res ||| (1L <<< pos)
        | x -> failwithf "Invalid bit %i. Mask: %A" x mask) value

let rec partiallyApplyMask2 mask value =
    let indexedMask = List.indexed mask

    let adressWithXBeingZeroed =
        indexedMask
        |> Seq.fold (fun res (pos, bit) ->
            match bit with
            | Zero -> res
            | One -> res ||| (1L <<< pos)
            | X -> res &&& ~~~(1L <<< pos)) value

    (adressWithXBeingZeroed,
     indexedMask
     |> List.choose (function
         | i, X -> Some i
         | _ -> None))

let writeMemory comp address value mask =
    let newMemory =
        comp.Memory
        |> Map.add address (applyMask mask value)

    { comp with Memory = newMemory }

let generateOptions bits = // [1; 2; 5]
    bits
    |> List.map (fun n -> 1L <<< n)
    |> List.fold (fun acc bit -> acc @ (acc |> List.map (fun n -> n + bit))) [ 0L ]

let resolveAddresses address mask =
    let baselineAddress, floatingMask = partiallyApplyMask2 mask address
    let addressOptions = generateOptions floatingMask

    addressOptions |> List.map ((+) baselineAddress)

let multiWriteMemory comp value addresses =
    let newMemory =
        addresses
        |> List.fold (fun mem address -> Map.add address value mem) comp.Memory

    { comp with Memory = newMemory }

let run comp instructions =
    let folder comp instruction =
        match instruction with
        | Mask mask -> { comp with Mask = mask }
        | Op (address, value) -> writeMemory comp address value comp.Mask

    Seq.fold folder comp instructions

let run2 comp instructions =
    let folder comp instruction =
        match instruction with
        | Mask mask -> { comp with Mask = mask }
        | Op (address, value) ->
            resolveAddresses address comp.Mask
            |> multiWriteMemory comp value

    Seq.fold folder comp instructions

let parseLine line =
    match line with
    | Scan "mem[%i] = %i" (address, value) -> Op (int64 address) (int64 value)
    | Scan "mask = %s" str -> Mask str
    | str -> failwithf "Invalid input: %s" str

let parseInput input = input |> split "\n" |> Seq.map parseLine

let initialComp =
    { Mask = List.empty
      Memory = Map.empty }

let day14Part1Solution =
    let result = parseInput day14Input |> run initialComp

    result.Memory |> Map.toSeq |> Seq.sumBy snd

let day14Part2Solution =
    let result =
        parseInput day14Input |> run2 initialComp

    result.Memory |> Map.toSeq |> Seq.sumBy snd

printfn "Day 14 part 1 solution: %i" day14Part1Solution
printfn "Day 14 part 2 solution: %i" day14Part2Solution
