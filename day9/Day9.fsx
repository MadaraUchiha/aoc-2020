#load "../common/Utils.fsx"
#load "Day9Input.fsx"
open Utils
open Day9Input

type EncryptionDatum = Set<int64> * int64

let toEncryptionData preambleSize numbers =
    numbers
    |> List.indexed
    |> Seq.filter (fun (i, _) -> i >= preambleSize)
    |> Seq.map (fun (i, datum) -> (Set numbers.[i - preambleSize..i - 1], datum))

let findPairSummingTo n set =
    set
    |> Seq.tryFind (fun a -> set |> Set.contains (n - a))

let validEncryptionDatum (preamble, datum) =
    findPairSummingTo datum preamble |> Option.isSome

let parseInput input = input |> split "\n" |> List.map int64

let preambleSize = 25

let findWeakness (weakDatum: int64) (data: int64 list) =
    let rec folder currentPreambleSize =
        if currentPreambleSize >= Seq.length data then
            None
        else
            let result =
                data
                |> toEncryptionData currentPreambleSize
                |> Seq.map (fst)
                |> Seq.tryFind (fun preambleSet -> (Set.fold (+) 0L preambleSet) = weakDatum)


            match result with
            | Some found -> Some(Set.maxElement found + Set.minElement found)
            | None -> folder (currentPreambleSize + 1)

    folder 2

let day9Part1Solution =
    parseInput day9Input
    |> toEncryptionData preambleSize
    |> Seq.tryFind (validEncryptionDatum >> not)

let day9Part2Solution =
    match day9Part1Solution with
    | None -> None
    | Some (_, weakness) -> parseInput day9Input |> findWeakness weakness

match day9Part1Solution with
| Some solution -> printfn "Invalid datum found: %i" (solution |> snd)
| None -> printfn "Did not find invalid datum."

match day9Part2Solution with
| Some solution -> printfn "Encryption Weakness: %i" (solution)
| None -> printfn "Did not find encryption weakness."
