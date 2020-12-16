#load "Day16Input.fsx"
#load "../common/Utils.fsx"

open Day16Input
open Utils

type Field =
    { Name: string
      Range: (int * int) list }

type Ticket = int list


let parseInput input =
    let parseField line =
        match line with
        | Scan "%s: %i-%i or %i-%i" (name, minA, maxA, minB, maxB) ->
            { Name = name
              Range = [ (minA, maxA); (minB, maxB) ] }
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

let inline inRange min max n = (n >= min) && (n <= max)

let isInvalid fields n =
    fields
    |> List.map (fun field -> field.Range)
    |> List.forall (fun ranges ->
        ranges
        |> List.forall (not << (fun (min, max) -> inRange min max n)))

let findInvalidFields fields (ticket: Ticket) =
    let fieldInvalid = isInvalid fields
    ticket |> List.filter fieldInvalid

let eliminateInvalidTickets fields tickets =
    tickets
    |> List.filter (fun ticket -> ticket |> findInvalidFields fields |> List.isEmpty)

let getTicketColumn (i: int) (tickets: Ticket list) = tickets |> List.map (fun t -> t.[i])

let findPotentialFieldsForColumn fields col =
    fields
    |> List.filter (fun field ->
        col
        |> List.forall (fun n ->
            let [ (minA, maxA); (minB, maxB) ] = field.Range

            (inRange minA maxA n) || (inRange minB maxB n)))

let playEliminations (columnMapping: (int * Field list) list) =
    let rec folder (acc: Map<int, Field>) (subMapping: (int * Field list) list) =
        match subMapping with
        | [] -> acc
        | (index, [ field ]) :: tail ->
            let mapWithFound = Map.add index field acc

            let tailWithEliminated =
                tail
                |> Seq.mapSnd (List.filter ((<>) field))
                |> List.ofSeq

            folder mapWithFound tailWithEliminated
        | _ ->
            failwith
                "Assumption failure: It is assumed that the column mapping matches increasing amounts of fields, so that the first element after filtering is always singular."


    folder Map.empty columnMapping

let day16Part1Solution =
    let (fields, mine, nearby) = parseInput day16Input

    nearby
    |> List.sumBy (fun ticket -> findInvalidFields fields ticket |> List.sum)

let inline printid x =
    printfn "%A" x
    x

let day16Part2Solution =
    let (fields, mine, nearby) = parseInput day16Input

    let validTickets = eliminateInvalidTickets fields nearby
    [ 0 .. (List.length fields) - 1 ]
    |> List.map (fun i ->
        let col = getTicketColumn i validTickets
        findPotentialFieldsForColumn fields col)
    |> List.indexed
    |> List.sortBy (snd >> List.length)
    |> playEliminations
    |> Map.filter (fun _ field -> field.Name.StartsWith("departure"))
    |> Map.toList
    |> List.map (fst >> (fun i -> mine.[i]) >> int64)
    |> List.reduce (*)
