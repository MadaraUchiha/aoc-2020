open System.Numerics

let modInv a m = BigInteger.ModPow(a, m - 2I, m)

let chineseRemainderTheorem equations =
    let product =
        equations |> Seq.map snd |> Seq.reduce (*)

    let res =
        equations
        |> Seq.map (fun (a, p) ->
            let n = product / p
            let inv = modInv p n
            (a * n * inv))
        |> Seq.reduce (+)

    res % product
