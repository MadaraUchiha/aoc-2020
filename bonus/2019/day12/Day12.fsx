#load "../../../common/Utils.fsx"

open Utils

let day12Input = "<x=4, y=12, z=13>
<x=-9, y=14, z=-3>
<x=-7, y=-1, z=2>
<x=-11, y=17, z=-1>"

[<StructuredFormatDisplayAttribute("<x={x}, y={y}, z={z}>")>]
type Vector3 =
    { x: int64
      y: int64
      z: int64 }
    static member add v1 v2 =
        { x = v1.x + v2.x
          y = v1.y + v2.y
          z = v1.z + v2.z }

    static member gravityFrom v1 v2 =
        { x = sign (v2.x - v1.x) |> int64
          y = sign (v2.y - v1.y) |> int64
          z = sign (v2.z - v1.z) |> int64 }

    static member inverse v = { x = -v.x; y = -v.y; z = -v.z }

    static member ofTuple(x, y, z) = { x = x; y = y; z = z }

    static member toEnergy v = (abs v.x) + (abs v.y) + (abs v.z)


let zeroVector = { x = 0L; y = 0L; z = 0L }

[<StructuredFormatDisplayAttribute("pos={Position}, vel={Velocity}")>]
type Moon =
    { Position: Vector3
      Velocity: Vector3 }

    static member ofPosition pos =
        { Position = pos
          Velocity = zeroVector }

    static member position moon = moon.Position
    static member velocity moon = moon.Velocity

    static member energy moon =
        (Vector3.toEnergy moon.Position)
        * (Vector3.toEnergy moon.Velocity)

    static member ofTuple = Vector3.ofTuple >> Moon.ofPosition

let motionsOf (moons: Moon list) =
    let calculateGravity moons moon =
        moons
        |> List.map (Moon.position >> Vector3.gravityFrom moon.Position)
        |> List.reduce Vector3.add

    seq {
        yield moons

        yield!
            Seq.unfold
                (fun currMoonsStep ->
                    // Gravity
                    let newMoonsStep =
                        currMoonsStep
                        |> List.map
                            (fun moon ->
                                let acceleration = calculateGravity currMoonsStep moon
                                let newVelocity = Vector3.add moon.Velocity acceleration

                                { Velocity = newVelocity
                                  Position = Vector3.add moon.Position newVelocity })

                    Some(newMoonsStep, newMoonsStep))
                moons
    }

let calculateEnergy system =
    system |> List.map Moon.energy |> List.reduce (+)


let parseInput input =
    input
    |> split "\n"
    |> List.map (sscanf "<x=%i, y=%i, z=%i>" >> Moon.ofTuple)

let day12Part1Solution =
    day12Input
    |> parseInput
    |> motionsOf
    |> Seq.item 1000
    |> calculateEnergy


let toDimensions moons =
    let xDim =
        moons |> Seq.map (fun m -> m.x, 0L) |> Array.ofSeq

    let yDim =
        moons |> Seq.map (fun m -> m.y, 0L) |> Array.ofSeq

    let zDim =
        moons |> Seq.map (fun m -> m.z, 0L) |> Array.ofSeq

    [| xDim; yDim; zDim |]

let getAccel x y = sign (y - x) |> int64

let step axis =
    axis
    |> Array.map (fun p -> Array.fold (fun (p1, v1) (p2, _) -> (p1, v1 + getAccel p1 p2)) p axis)
    |> Array.map (fun (p, v) -> (p + v, v))

let stepsUntilEqual axis =
    let rec fold n dim' =
        if axis = dim' && n > 0L then n else step dim' |> fold (n + 1L)

    fold 0L axis

let rec greatestCommonDivisor a b =
    if b = 0L then abs a else greatestCommonDivisor b (a % b)

let leastCommonMultiplier a b = a * b / (greatestCommonDivisor a b)

let day12Part2Solution =
    day12Input
    |> parseInput
    |> Seq.map Moon.position
    |> toDimensions
    |> Array.map stepsUntilEqual
    |> Array.reduce leastCommonMultiplier
