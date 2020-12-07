#load "../common/Utils.fsx"
#load "Day7Input.fsx"
#r "nuget: FSharpPlus"
open Day7Input
open FSharpPlus
open Utils

type Bag = Bag of string

type BagNode =
    { bag: Bag
      contains: (int * Bag) list
      containedIn: Set<BagNode> }

let parseContainedBags (str: string) =
    str.Trim()
    |> split ", "
    |> List.map (fun line ->
        match line with
        | RegExp.Pattern @"(\d+) ([\w\s]+) bags?" [ number; color ] -> (int number, Bag color)
        | _ -> failwith "Invalid input")


let parseBagContains str =
    let parsedLine = str |> sscanf "%s bags contain %s"
    match parsedLine with
    | color, "no other bags" ->
        { bag = Bag color
          contains = []
          containedIn = Set.empty }
    | color, bags ->
        { bag = Bag color
          contains = parseContainedBags bags
          containedIn = Set.empty }

let findContained { bag = bag } bagList =
    bagList
    |> List.filter (fun { contains = contains } -> contains |> List.map snd |> List.contains bag)
    |> Set.ofList

let parseInput input =
    let bagsContaining =
        input
        |> split "\n"
        |> List.map
            ((fun line -> line.TrimEnd('.'))
             >> parseBagContains)

    let bagsContained =
        bagsContaining
        |> List.map (fun node ->
            { node with
                  containedIn = findContained node bagsContaining })

    bagsContained

let findBag nodes bag =
    nodes
    |> List.find (fun { bag = listBag } -> listBag = bag)

let searchAncestors list item =
    let rec search acc item: Set<BagNode> =
        let foundThisTime =
            item.containedIn
            |> Set.map (fun foundItem -> findBag list foundItem.bag)

        match Set.count foundThisTime with
        | 0 -> acc
        | _ ->
            foundThisTime
            |> Set.map (fun foundItem -> search acc foundItem)
            |> Set.unionMany
            |> Set.union foundThisTime
            |> Set.union acc

    search Set.empty item

let searchDescendants list bagNode =
    let rec sum { contains = contains } =
        let mySum = contains |> List.sumBy fst

        let descendants =
            contains
            |> List.map (fun (count, bag) -> (count, findBag list bag))
            |> List.sumBy (fun (count, bagNode) -> count * sum bagNode)

        mySum + descendants

    sum bagNode

let parsedInput = parseInput day7Input

let day7Part1Solution =
    parsedInput
    |> List.find (fun { bag = b } -> b = (Bag "shiny gold"))
    |> searchAncestors parsedInput
    |> Set.count

let day7Part2Solution =
    parsedInput
    |> List.find (fun { bag = b } -> b = (Bag "shiny gold"))
    |> searchDescendants parsedInput

printfn "Day 7 part 1 solution: %i" day7Part1Solution
printfn "Day 7 part 2 solution: %i" day7Part2Solution
