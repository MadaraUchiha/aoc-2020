#load "../common/Utils.fsx"

open Utils

let player1PublicKey = 2069194L
let player2PublicKey = 16426071L

let magicMod = 20201227L

let encryptionStep subject n = (n * subject) % magicMod

let findLoopsTill finish =
    let rec find' curr n =
        if curr = finish then n else find' (encryptionStep 7L curr) (n + 1)

    find' 1L 0

let encrypt subjectNumber loops =
    let rec encrypt' curr finish =
        if finish = 0
        then curr
        else encrypt' (encryptionStep curr subjectNumber) (finish - 1)

    encrypt' 1L loops

let day25Part1Solution =
    let player1Loops = findLoopsTill player1PublicKey

    encrypt player2PublicKey player1Loops

printfn "Day 25 part 1 solution: %i" day25Part1Solution
