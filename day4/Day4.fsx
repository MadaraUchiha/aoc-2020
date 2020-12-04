open System
open System.Text.RegularExpressions

#load "Day4Input.fsx"
open Day4Input

module Year =
    type T = Year of int


    let valid from thru (Year year) = year >= from && year <= thru

    let create year = Year year

module Height =
    type T = Height of (int * string)

    let create (height: string) =
        let numeric =
            if (height.Length > 2) then height.[..(height.Length - 3)] else "0"

        let unit =
            height.[(height.Length - 2)..height.Length]

        Height(int numeric, unit)


    let valid (Height height) =
        let (height, units) = height


        match units with
        | "cm" -> height >= 150 && height <= 193
        | "in" -> height >= 59 && height <= 76
        | _ -> false

module Color =
    type T = Color of string
    let create color = Color color

    let valid (Color color) = Regex.IsMatch(color, @"^#[0-9a-f]{6}$")

module EyeColor =
    type T = EyeColor of string
    let create color = EyeColor color

    let valid (EyeColor color) =
        match color with
        | "amb"
        | "blu"
        | "brn"
        | "gry"
        | "grn"
        | "hzl"
        | "oth" -> true
        | _ -> false

module PassportId =
    type T = PassportId of string
    let create pid = PassportId pid

    let valid (PassportId pid) = Regex.IsMatch(pid, @"^\d{9}$")

type Passport =
    { byr: Year.T option
      iyr: Year.T option
      eyr: Year.T option
      hgt: Height.T option
      hcl: Color.T option
      ecl: EyeColor.T option
      pid: PassportId.T option
      cid: string option }


let emptyPassport =
    { byr = None
      iyr = None
      eyr = None
      hgt = None
      hcl = None
      ecl = None
      pid = None
      cid = None }

let isPassportPart1Valid passport =
    match passport with
    | { byr = Some _; iyr = Some _; eyr = Some _; hgt = Some _; hcl = Some _; ecl = Some _; pid = Some _ } -> true
    | _ -> false

let isPassportPart2Valid passport =
    match passport with
    | { byr = Some byr; iyr = Some iyr; eyr = Some eyr; hgt = Some hgt; hcl = Some hcl; ecl = Some ecl; pid = Some pid } ->
        Year.valid 1920 2002 byr
        && Year.valid 2010 2020 iyr
        && Year.valid 2020 2030 eyr
        && Height.valid hgt
        && Color.valid hcl
        && EyeColor.valid ecl
        && PassportId.valid pid
    | _ -> false

let passportEntryOfString passport (str: string) =
    let keyValue = str.Trim().Split(':')
    match keyValue with
    | [| key; value |] ->
        match key with
        | "byr" ->
            { passport with
                  byr = Some(Year.create (int value)) }
        | "iyr" ->
            { passport with
                  iyr = Some(Year.create (int value)) }
        | "eyr" ->
            { passport with
                  eyr = Some(Year.create (int value)) }
        | "hgt" ->
            { passport with
                  hgt = Some(Height.create value) }
        | "hcl" ->
            { passport with
                  hcl = Some(Color.create value) }
        | "ecl" ->
            { passport with
                  ecl = Some(EyeColor.create value) }
        | "pid" ->
            { passport with
                  pid = Some(PassportId.create value) }
        | "cid" -> { passport with cid = Some value }
        | key -> failwith (sprintf "Invalid key: %s." key)
    | _ -> failwith (sprintf "Invalid input. Line: %s" str)

let passportOfString (str: string) =
    str.Trim().Split([| '\n'; ' ' |], StringSplitOptions.None)
    |> Array.fold passportEntryOfString emptyPassport

let parseInput (input: string) =
    input.Trim().Split([| "\n\n" |], StringSplitOptions.None)
    |> List.ofArray
    |> List.map passportOfString

let day4Part1Solution =
    parseInput day4Input
    |> List.filter isPassportPart1Valid
    |> List.length

let day4Part2Solution =
    parseInput day4Input
    |> List.filter isPassportPart2Valid
    |> List.length

printfn "Day 4 part 1 solution: %i" day4Part1Solution
printfn "Day 4 part 2 solution: %i" day4Part2Solution
