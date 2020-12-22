#load "../common/Utils.fsx"
open Utils

let day22Input = "Player 1:
10
39
16
32
5
46
47
45
48
26
36
27
24
37
49
25
30
13
23
1
9
3
31
14
4

Player 2:
2
15
29
41
11
21
8
44
38
19
12
20
40
17
22
35
34
42
50
6
33
7
18
28
43"

type GameOutcome =
    | Player1 of deck: int list
    | Player2 of deck: int list

let play deck1 deck2 =
    let rec playFold deck1 deck2 =
        match deck1, deck2 with
        | _, [] -> Player1 deck1
        | [], _ -> Player2 deck2
        | head1 :: tail1, head2 :: tail2 ->
            if head1 < head2
            then playFold tail1 (tail2 @ [ head2; head1 ])
            else playFold (tail1 @ [ head1; head2 ]) tail2

    playFold deck1 deck2

let rec playRecursive deck1 deck2 =
    let rec playFold player1PreviousHands player2PreviousHands deck1 deck2 =
        if Set.contains deck1 player1PreviousHands
           || Set.contains deck2 player2PreviousHands then
            Player1 deck1
        else
            match deck1, deck2 with
            | _, [] -> Player1 deck1
            | [], _ -> Player2 deck2
            | head1 :: tail1, head2 :: tail2 ->
                let newP1PrevHands = Set.add deck1 player1PreviousHands
                let newP2PrevHands = Set.add deck2 player2PreviousHands
                let p1Remaining = List.length tail1
                let p2Remaining = List.length tail2

                if p1Remaining >= head1 && p2Remaining >= head2 then
                    // printfn "Recurse! %A %A" d1.[0..c1 - 1] d2.[0..c2 - 1]
                    match playRecursive tail1.[0..head1 - 1] tail2.[0..head2 - 1] with
                    | Player1 _ -> playFold newP1PrevHands newP2PrevHands (tail1 @ [ head1; head2 ]) tail2
                    | Player2 _ -> playFold newP1PrevHands newP2PrevHands tail1 (tail2 @ [ head2; head1 ])
                else if head1 < head2 then
                    playFold newP1PrevHands newP2PrevHands tail1 (tail2 @ [ head2; head1 ])
                else
                    playFold newP1PrevHands newP2PrevHands (tail1 @ [ head1; head2 ]) tail2

    playFold Set.empty Set.empty deck1 deck2

let calculateScore deck =
    match deck with
    | Player1 deck
    | Player2 deck ->
        deck
        |> Seq.rev
        |> Seq.indexed
        |> Seq.mapFst ((+) 1)
        |> Seq.sumBy (fun (i, c) -> i * c)

let parseDeck deck =
    deck
    |> split "\n"
    |> Seq.tail
    |> Seq.map int
    |> Seq.toList

let parseInput input =
    match input |> split "\n\n" with
    | [ p1; p2 ] -> parseDeck p1, parseDeck p2
    | _ -> failwith "Invalid input."

let day22Part1Solution =
    day22Input
    |> parseInput
    ||> play
    |> calculateScore

let day22Part2Solution =
    day22Input
    |> parseInput
    ||> playRecursive
    |> calculateScore

printfn "Day 22 part 1 solution: %i" day22Part1Solution
printfn "Day 22 part 2 solution: %i" day22Part2Solution
