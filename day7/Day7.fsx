#load "../common/Utils.fsx"
#load "Day7Input.fsx"
#r "nuget: FSharpPlus"
open Day7Input
open FSharpPlus
open Utils

type BagColor = BagColor of string

type LuggageRule =
    { Color: BagColor
      Contains: Map<BagColor, int> }

let parseContainedBags (str: string) =
    str
    |> trim
    |> split ", "
    |> Seq.map (function
        | RegExp.Pattern @"(\d+) ([\w\s]+) bags?" [ number; color ] -> (BagColor color, int number)
        | _ -> failwith "Invalid input")
    |> Map.ofSeq

let parseBagContains str =
    str
    |> sscanf "%s bags contain %s"
    |> (function
    | color, "no other bags" ->
        { Color = BagColor color
          Contains = Map.empty }
    | color, bags ->
        { Color = BagColor color
          Contains = parseContainedBags bags })

let addToSetInMapByKey key value map =
    match Map.tryFind key map with
    | Some set -> map |> Map.add key (Set.add value set)
    | None -> map |> Map.add key (Set [ value ])

let calculateParentalMappings rules =
    rules
    |> Seq.fold (fun acc rule ->
        rule.Contains
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.fold (fun partialAcc color -> addToSetInMapByKey color rule.Color partialAcc) acc) Map.empty

let findEventuallyContaining bag parentalMapping =
    let rec folder acc bag =
        match Map.tryFind bag acc with
        | None -> Set [ bag ]
        | Some set ->
            set
            |> Seq.map (folder acc)
            |> Seq.reduce Set.union
            |> Set.add bag

    folder parentalMapping bag |> Set.remove bag


let totalBagsContainedIn color list =
    let containingMapping =
        list
        |> Seq.map (fun rule -> (rule.Color, rule.Contains |> Map.toSeq))
        |> Map.ofSeq

    let rec sum bag =
        match Map.tryFind bag containingMapping with
        | None -> 0
        | Some containing ->
            containing
            |> Seq.fold (fun acc (color, count) -> acc + (count * sum color)) 1

    (sum color) - 1

let myBag = BagColor "shiny gold"

let parseInput input =
    input
    |> split "\n"
    |> Seq.map ((trimEnd '.') >> parseBagContains)

let day7Part1Solution =
    parseInput day7Input
    |> calculateParentalMappings
    |> findEventuallyContaining myBag
    |> Set.count

let day7Part2Solution =
    parseInput day7Input |> totalBagsContainedIn myBag

printfn "Day 7 part 1 solution: %i" day7Part1Solution
printfn "Day 7 part 2 solution: %i" day7Part2Solution
