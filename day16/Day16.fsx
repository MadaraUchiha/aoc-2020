#load "Day16Input.fsx"
#load "../common/Utils.fsx"

open Day16Input
open Utils

type Field =
    { Name: string
      Range: (int * int) * (int * int) }

type Ticket = int list


let parseInput input =
    let parseField line =
        match line with
        | Scan "%s: %i-%i or %i-%i" (name, minA, maxA, minB, maxB) ->
            { Name = name
              Range = (minA, maxA), (minB, maxB) }
        | str -> failwithf "Invalid field string: %s" str

    let parseTicket line = line |> split "," |> List.map int

    let sections = input |> split "\n\n"

    match sections with
    | [ fields; mine; nearby ] ->
        let parsedFields =
            fields |> split "\n" |> List.map parseField

        let parsedMyTicket: Ticket =
            (mine |> split "\n").[1] // Ignore first title element
            |> parseTicket

        let parsedNearbyTickets: Ticket list =
            (nearby |> split "\n")
            |> List.tail // Ignore first title element
            |> List.map parseTicket

        (parsedFields, parsedMyTicket, parsedNearbyTickets)

    | _ -> failwith "Invalid input."

let inRange min max n = (n >= min) && (n <= max)

let inEitherRange range1 range2 n =
    (range1 ||> inRange) n || (range2 ||> inRange) n

let isInvalid fields n =
    fields
    |> List.map (fun field -> field.Range)
    |> List.forall (fun ranges -> not (n |> (inEitherRange <|| ranges)))

let findInvalidFields fields (ticket: Ticket) =
    let fieldInvalid = isInvalid fields
    ticket |> List.filter fieldInvalid

let eliminateInvalidTickets fields tickets =
    tickets
    |> List.filter (findInvalidFields fields >> List.isEmpty)

let getTicketColumn (tickets: Ticket list) i = tickets |> List.map (fun t -> t.[i])

let findPotentialFieldsForColumn fields col =
    fields
    |> List.filter (fun field -> col |> List.forall (inEitherRange <|| field.Range))

let playEliminations columnMapping =
    let rec eliminate acc subMapping =
        match subMapping with
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

let day16Part1Solution =
    let (fields, _, nearby) = parseInput day16Input

    nearby
    |> List.sumBy (findInvalidFields fields >> List.sum)

let day16Part2Solution =
    let (fields, mine, nearby) = parseInput day16Input

    let validTickets = eliminateInvalidTickets fields nearby

    let getColumnByIndex = getTicketColumn validTickets
    let findMatchingFields = findPotentialFieldsForColumn fields
    let getMyFieldByIndex i = mine.[i]

    seq [ 0 .. (List.length fields) - 1 ] // for each column
    |> Seq.map (getColumnByIndex >> findMatchingFields) // find potential matches
    |> Seq.indexed // remember the original indices
    |> Seq.sortBy (snd >> List.length) // sort by number of potential fields
    |> Seq.toList
    |> playEliminations // Magic~~
    |> Map.filter (fun _ field -> field.Name.StartsWith("departure"))
    |> Map.toSeq
    |> Seq.map (fst >> getMyFieldByIndex >> int64)
    |> Seq.reduce (*)

printfn "Day 16 part 1 solution: %i" day16Part1Solution
printfn "Day 16 part 2 solution: %i" day16Part2Solution
