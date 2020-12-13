let modularInverse n g =
    let rec fN n i g e l a =
        match e with
        | 0L -> g
        | _ ->
            let o = n / e
            fN e l a (n - o * e) (i - o * l) (g - o * a)

    (n + (fN n 1L 0L g 0L 1L)) % n

let chineseRemainderTheorem equations =
    let product =
        equations |> Seq.map snd |> Seq.reduce (*)

    let res =
        equations
        |> Seq.map (fun (a, p) ->
            let n = product / p
            let inv = modularInverse p n
            (a * n * inv))
        |> Seq.reduce (+)

    res % product
