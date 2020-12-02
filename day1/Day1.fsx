#load "Day1Input.fsx"
open Day1Input

type SumExpression = int * int * int
type TripleSumExpression = int * int * int * int

let parseInput (x: string) =
    x.Split([| '\n' |]) |> Set.ofArray |> Set.map int

let generateSums ns =
    seq {
        for a in ns do
            yield!
                seq {
                    for b in ns do
                        yield SumExpression(a, b, a + b)
                }
    }

let generateTripleSums ns =
    seq {
        for a in ns do
            yield!
                seq {
                    for b in ns do
                        yield!
                            seq {
                                for c in ns do
                                    yield TripleSumExpression(a, b, c, a + b + c)
                            }
                }
    }

let day1Part1Solution =
    parseInput day1Input
    |> generateSums
    |> Seq.tryFind (fun (_, _, res) -> res = 2020)

let day1Part2Solution =
    parseInput day1Input
    |> generateTripleSums
    |> Seq.tryFind (fun (_, _, _, res) -> res = 2020)

match day1Part1Solution with
| Some (a, b, sum) -> printfn "%i + %i = %i, %i * %i = %i" a b sum a b (a * b)
| None -> do printfn "Part1: Not found."

match day1Part2Solution with
| Some (a, b, c, sum) -> printfn "%i + %i + %i = %i, %i * %i * %i = %i" a b c sum a b c (a * b * c)
| None -> do printfn "Part2: Not found."
