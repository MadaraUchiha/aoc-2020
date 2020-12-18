#load "../common/Utils.fsx"
#load "Day18Input.fsx"

open Utils
open Day18Input

type Value =
    | Number of int
    | Formula of string

type Operator =
    | Mul
    | Add

type Expression = Operator * Value

type ParserState =
    { Braces: int
      Result: Expression list
      FormingExpression: string
      CurrentOperator: Operator }

let parseExpression (exp: string) =
    let folder acc c =
        let sc = string c
        match acc.Braces, sc with
        | n, _ when n < 0 -> failwith "Imbalanced brackets."
        | 1, ")" ->
            { acc with
                  Braces = 0
                  Result =
                      acc.Result
                      @ [ acc.CurrentOperator, Formula acc.FormingExpression ]
                  FormingExpression = "" }
        | n, "(" when n > 0 ->
            { acc with
                  FormingExpression = acc.FormingExpression + sc
                  Braces = acc.Braces + 1 }
        | n, ")" when n > 0 ->
            { acc with
                  FormingExpression = acc.FormingExpression + sc
                  Braces = acc.Braces - 1 }
        | n, _ when n > 0 ->
            { acc with
                  FormingExpression = acc.FormingExpression + sc }
        | _, Scan "%i" i ->
            { acc with
                  Result = acc.Result @ [ acc.CurrentOperator, Number i ] }
        | _, "+" -> { acc with CurrentOperator = Add }
        | _, "*" -> { acc with CurrentOperator = Mul }
        | _, "(" -> { acc with Braces = acc.Braces + 1 }
        | _, ")" -> { acc with Braces = acc.Braces - 1 }
        | _, " " -> acc
        | _, other -> failwithf "Invalid expression character %s" other

    let initialParserState =
        { Braces = 0
          Result = List.empty
          FormingExpression = ""
          CurrentOperator = Add }

    let result =
        exp |> Seq.fold folder initialParserState

    result.Result


let rec solve expressions =
    expressions
    |> Seq.fold (fun acc exp ->
        match exp with
        | Add, Number n -> acc + (int64 n)
        | Add, Formula s -> acc + (solve (parseExpression s))
        | Mul, Number n -> acc * (int64 n)
        | Mul, Formula s -> acc * (solve (parseExpression s))) 0L

let part2ify =
    (RegExp.replace @"\(" "(("
     >> RegExp.replace @"\)" "))"
     >> RegExp.replace @"\*" ") * (")
    >> (fun s -> "( " + s + " )")

let parseInputWith mapper input =
    input
    |> split "\n"
    |> Seq.map (mapper >> parseExpression)

let day18Part1Solution =
    day18Input |> parseInputWith id |> Seq.sumBy solve

let day18Part2Solution =
    day18Input
    |> parseInputWith part2ify
    |> Seq.sumBy solve

printfn "Day 18 part 1 solution: %i" day18Part1Solution
printfn "Day 18 part 2 solution: %i" day18Part2Solution
