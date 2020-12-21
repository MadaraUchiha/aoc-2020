#load "../common/Utils.fsx"
#load "Day21Input.fsx"

open Utils
open Day21Input

type Food =
    { IngredientNames: Set<string>
      Allergens: Set<string> }

let toFood (ingredients, allergens) =
    { IngredientNames = ingredients
      Allergens = allergens }

let ingredients food = food.IngredientNames
let allergens food = food.Allergens

let associateIngredients foods =
    let findFoodsFor allergen =
        foods
        |> Seq.filter (fun f -> Set.contains allergen f.Allergens)
        |> Seq.map (fun f -> f.IngredientNames)

    let allAllergens =
        foods
        |> Seq.map (fun f -> f.Allergens)
        |> Set.unionMany

    allAllergens
    |> Seq.map (fun a -> a, findFoodsFor a |> Set.intersectMany)

let findFoodsWithPossibleAllergens foods =
    let associations = associateIngredients foods

    associations |> Seq.map snd |> Set.unionMany

let findFoodsWithoutAllergens foods =
    let associations = associateIngredients foods

    let allFoods =
        foods
        |> Seq.map (fun f -> f.IngredientNames)
        |> Set.unionMany

    let allFoodsWithPossibleAllergens =
        associations |> Seq.map snd |> Set.unionMany

    Set.difference allFoods allFoodsWithPossibleAllergens

let countOccurrences haystack needles =
    needles
    |> Seq.sumBy (fun needle ->
        haystack
        |> Seq.filter (Set.contains (needle))
        |> Seq.length)

let playEliminations columnMapping = // day 16 comeback babyyyyyy
    let rec eliminate acc subMapping =
        match subMapping |> List.sortBy (snd >> List.length) with
        | [] -> acc
        | (index, [ field ]) :: tail ->
            let mapWithFound = Map.add index field acc

            let tailWithEliminated =
                tail
                |> Seq.mapSnd (List.filter ((<>) field))
                |> List.ofSeq

            eliminate mapWithFound tailWithEliminated
        | _ ->
            failwith "Assumption failure: It is assumed that the column mapping matches increasing amounts of fields,
                so that the first element after filtering is always singular."

    eliminate Map.empty columnMapping


let parseInput input =
    input
    |> split "\n"
    |> Seq.map
        ((sscanf "%s (contains %s)")
         >> (fun (ings, alls) -> (ings |> split " ", alls |> split ", ")))
    |> Seq.mapBoth Set.ofSeq
    |> Seq.map toFood
    |> Seq.toList

let day21Part1Solution =
    let foodList = day21Input |> parseInput

    foodList
    |> findFoodsWithoutAllergens
    |> countOccurrences (foodList |> Seq.map ingredients)

let day21Part2Solution =
    day21Input
    |> parseInput
    |> associateIngredients
    |> Seq.mapSnd Set.toList
    |> Seq.sortBy (snd >> List.length)
    |> Seq.toList
    |> playEliminations
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map snd
    |> String.concat ","

printfn "Day 21 part 1 solution: %i" day21Part1Solution
printfn "Day 21 part 2 solution: %s" day21Part2Solution
