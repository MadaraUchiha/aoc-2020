#load "Day2Input.fsx"
#r "nuget: FSharpPlus"
open FSharpPlus
open System.Text.RegularExpressions
open Day2Input

let countMatches needle haystack =
    Regex.Matches(haystack, Regex.Escape needle).Count

module PasswordPolicy =
    type PasswordPolicy =
        { Min: int
          Max: int
          Letter: char
          Password: string }

    let ofTuple (min, max, letter, password) =
        { Min = min
          Max = max
          Letter = letter
          Password = password }

    let isValidPasswordPolicyAccordingToPart1 { Min = min; Max = max; Password = password; Letter = letter } =
        let occurences = countMatches (letter |> string) password

        (occurences >= min) && (occurences <= max)

    let private fromElivshIndex i = i - 1

    let isValidPasswordPolicyAccordingToPart2 { Min = min; Max = max; Password = password; Letter = letter } =
        let pos1 = password.[fromElivshIndex min]
        let pos2 = password.[fromElivshIndex max]

        (letter = pos1 && letter <> pos2)
        || (letter <> pos1 && letter = pos2)


let parseInput (input: string) =
    input.Split([| '\n' |])
    |> List.ofArray
    |> List.map (PasswordPolicy.ofTuple << (sscanf "%i-%i %c: %s"))

let day2Part1Solution =
    parseInput day2Input
    |> List.filter PasswordPolicy.isValidPasswordPolicyAccordingToPart1
    |> List.length

let day2Part2Solution =
    parseInput day2Input
    |> List.filter PasswordPolicy.isValidPasswordPolicyAccordingToPart2
    |> List.length

printfn
    "Day 2, part 1: There are %i valid passwords out of %i total."
    day2Part1Solution
    (parseInput day2Input |> List.length)


printfn
    "Day 2, part 1: There are %i valid passwords out of %i total."
    day2Part2Solution
    (parseInput day2Input |> List.length)
