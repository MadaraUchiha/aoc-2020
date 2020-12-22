open System.Numerics

#load "../../../common/Utils.fsx"
#load "Day22Input.fsx"
open Utils
open Day22Input

let modInv a m = BigInteger.ModPow(a, m - 2I, m)

type Transformation =
    { Scale: bigint
      Offset: bigint
      Mod: bigint }
    static member combine tf1 tf2 =
        { Scale = (tf1.Scale * tf2.Scale) %% tf1.Mod
          Offset = (tf2.Scale * tf1.Offset + tf2.Offset) %% tf1.Mod
          Mod = tf1.Mod }

    static member apply position tf =
        (tf.Scale * position + tf.Offset) %% tf.Mod

    static member inverse tf =
        let invScale = modInv tf.Scale tf.Mod
        { tf with
              Scale = invScale
              Offset = -(invScale * tf.Offset) %% tf.Mod }

    static member combineSelf tf = Transformation.combine tf tf

type Instruction =
    | DealWithIncrement of int
    | Cut of int
    | DealIntoNewStack
    static member toTransformation deckSize =
        function
        | DealWithIncrement x ->
            { Mod = deckSize
              Scale = bigint x
              Offset = 0I }
        | Cut i ->
            { Mod = deckSize
              Scale = 1I
              Offset = deckSize - bigint i }
        | DealIntoNewStack ->
            { Mod = deckSize
              Scale = -1I
              Offset = deckSize - 1I }

let combineToPower tf n =
    [ 1 .. n ]
    |> Seq.fold (fun acc _ -> Transformation.combineSelf acc) tf

let createEquivalentTransformation (n: bigint) tf =
    let binary = System.Convert.ToString(int64 n, 2)

    let activatedBits =
        binary
        |> Seq.rev
        |> Seq.indexed
        |> Seq.filter (snd >> ((=) '1'))
        |> Seq.map fst

    activatedBits
    |> Seq.map (combineToPower tf)
    |> Seq.reduce Transformation.combine

let parseInput input =
    input
    |> split "\n"
    |> Seq.map (function
        | Scan "deal with increment %i" i -> DealWithIncrement i
        | "deal into new stack" -> DealIntoNewStack
        | Scan "cut %i" i -> Cut i
        | other -> failwithf "Invalid instruction %s" other)

let day22Part1Solution =
    let deckSize = 10007I

    day22Input
    |> parseInput
    |> Seq.map (Instruction.toTransformation deckSize)
    |> Seq.reduce Transformation.combine
    |> Transformation.apply 2019I

let day22Part2Solution =
    let deckSize = 119315717514047I
    let times = 101741582076661I

    day22Input
    |> parseInput
    |> Seq.map (Instruction.toTransformation deckSize)
    |> Seq.reduce Transformation.combine
    |> Transformation.inverse
    |> createEquivalentTransformation times
    |> Transformation.apply 2020I
