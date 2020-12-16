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

let isInvalid fields n =
    fields
    |> List.map (fun field -> field.Range)
    |> List.forall (fun ((minA, maxA), (minB, maxB)) ->
        (not (inRange minA maxA n))
        && (not (inRange minB maxB n)))

let findInvalidFields fields (ticket: Ticket) =
    let fieldInvalid = isInvalid fields
    ticket |> List.filter fieldInvalid

let eliminateInvalidTickets fields tickets =
    tickets
    |> List.filter (fun ticket -> ticket |> findInvalidFields fields |> List.isEmpty)

let getTicketColumn (tickets: Ticket list) i = tickets |> List.map (fun t -> t.[i])

let findPotentialFieldsForColumn fields col =
    fields
    |> List.filter (fun field ->
        col
        |> List.forall (fun n ->
            let (minA, maxA), (minB, maxB) = field.Range

            (inRange minA maxA n) || (inRange minB maxB n)))

let playEliminations (columnMapping: (int * Field list) list) =
    let rec eliminate (acc: Map<int, Field>) (subMapping: (int * Field list) list) =
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
    |> List.sumBy (fun ticket -> findInvalidFields fields ticket |> List.sum)

let day16Part2Solution =
    let (fields, mine, nearby) = parseInput day16Input

    let validTickets = eliminateInvalidTickets fields nearby

    let getColumnByIndex = getTicketColumn validTickets
    [ 0 .. (List.length fields) - 1 ] // for each column
    |> List.map
        (getColumnByIndex
         >> findPotentialFieldsForColumn fields) // find potential matches
    |> List.indexed // remember the original indices
    |> List.sortBy (snd >> List.length) // sort by number of potential fields
    |> playEliminations // Magic~~
    |> Map.filter (fun _ field -> field.Name.StartsWith("departure"))
    |> Map.toList
    |> List.map (fst >> (fun i -> mine.[i]) >> int64)
    |> List.reduce (*)

printfn "Day 16 part 1 solution: %i" day16Part1Solution
printfn "Day 16 part 2 solution: %i" day16Part2Solution
