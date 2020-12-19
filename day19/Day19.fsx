#load "../common/Utils.fsx"
#load "Day19Input.fsx"

open Utils
open Day19Input
open System.Text.RegularExpressions

type Rule =
    | Literal of string
    | Reference of int
    | List of Rule list
    | Either of Rule * Rule
    | OneOrMore of Rule
    | BalancedPair of Rule * Rule

let rec parseRuleContextless rule =
    match rule with
    | Scan "\"%c\"" c -> Literal(string c)
    | Scan "%s | %s" (left, right) -> Either(parseRuleContextless left, parseRuleContextless right)
    | Scan "%i" i -> Reference i
    | Scan "%i %i" (a, b) -> List [ Reference a; Reference b ]
    | Scan "%i %i %i" (a, b, c) ->
        List [ Reference a
               Reference b
               Reference c ]
    | r -> failwithf "Unsupported rule structure %s" r

let resolveReferences ruleMap =
    let rec folder rule =
        match rule with
        | Literal _ -> rule
        | Reference r -> ruleMap |> Map.find r |> folder
        | Either (a, b) -> Either(folder a, folder b)
        | List rules -> List(rules |> List.map (folder))
        | OneOrMore rule -> OneOrMore(folder rule)
        | BalancedPair (a, b) -> BalancedPair(folder a, folder b)

    ruleMap |> Map.find 0 |> folder

let generateBalancedPair n a b =
    [ 1 .. n ]
    |> List.map (fun i -> sprintf "(?:%s)(?:%s)" (String.replicate i a) (String.replicate i b))
    |> String.concat "|"
    |> (fun s -> sprintf "(?:%s)" s)

let compileRegex rule =
    let arbitraryMaxManyPair = 5 // Trial and error! :D

    let rec generate rule =
        match rule with
        | List rules -> rules |> List.map generate |> List.reduce (+)
        | Either (a, b) -> sprintf "(?:%s|%s)" (generate a) (generate b)
        | Literal c -> c
        | OneOrMore r -> sprintf "(?:%s)+" (generate r)
        | BalancedPair (a, b) -> generateBalancedPair arbitraryMaxManyPair (generate a) (generate b)
        | Reference _ -> failwith "Should not have any unresolved references here."

    generate rule
    |> sprintf "^%s$"
    |> (fun s -> Regex(s, RegexOptions.Compiled))

let parseRules rules =
    rules
    |> List.map (sscanf "%i: %s")
    |> Seq.mapSnd parseRuleContextless
    |> Map.ofSeq

let parseInput input =
    match input |> split "\n\n" with
    | [ rules; messages ] -> rules |> split "\n" |> parseRules, messages |> split "\n"
    | _ -> failwith "Invalid input"

let day19Part1Solution =
    let rules, messages = parseInput day19Input

    let regex =
        rules |> resolveReferences |> compileRegex

    messages
    |> List.sumBy (fun msg -> if regex.IsMatch(msg) then 1 else 0)

let day19Part2Solution =
    let rules, messages = parseInput day19Input

    let editedRules =
        rules
        |> Map.add 8 (OneOrMore(Reference 42))
        |> Map.add 11 (BalancedPair(Reference 42, Reference 31))

    let regex =
        editedRules |> resolveReferences |> compileRegex

    messages
    |> List.sumBy (fun msg -> if regex.IsMatch(msg) then 1 else 0)

printfn "Day 19 part 1 solution: %i" day19Part1Solution
printfn "Day 19 part 2 solution: %i" day19Part2Solution
