#load "../common/Utils.fsx"

open Utils

let day23Input = "193467258"


let play moves cups =
    let maxCup = List.max cups
    let nextCup cup = if cup = 1 then maxCup else cup - 1

    let links = Array.init (moves + 1) (fun _ -> -1)
    links.[Seq.last cups] <- Seq.head cups
    for l, r in Seq.pairwise cups do
        links.[l] <- r

    let mutable current = Seq.head cups
    for _ in 0 .. moves - 1 do
        // Take out three cups next to current
        let mutable picked = [ links.[current] ]
        for _ in 0 .. 1 do
            picked <- picked @ [ links.[List.last picked] ]

        // Link current to the cup after the picked three
        links.[current] <- links.[List.last picked]

        // Select destination cup
        let mutable destination = nextCup current
        while List.contains destination picked do
            destination <- nextCup destination

        // Place three cups after destination
        let nextToDest = links.[destination]
        links.[destination] <- List.head picked
        links.[List.last picked] <- nextToDest
        current <- links.[current]

    seq {
        let mutable cup = 1
        yield cup
        while links.[cup] <> 1 do
            yield links.[cup]
            cup <- links.[cup]
    }

let day23Part1Solution =
    let input =
        day23Input |> Seq.map (charToInt) |> Seq.toList

    input
    |> play 100
    |> Seq.skip 1
    |> Seq.map string
    |> String.concat ""

let day23Part2Solution =
    let input =
        (day23Input |> Seq.map (charToInt) |> Seq.toList)
        @ [ 10 .. 1_000_000 ]

    input
    |> play 10_000_000
    |> Seq.skip 1
    |> Seq.take 2
    |> Seq.map int64
    |> Seq.reduce (*)

printfn "Day 23 part 1 solution: %s" day23Part1Solution
printfn "Day 23 part 2 solution: %i" day23Part2Solution
