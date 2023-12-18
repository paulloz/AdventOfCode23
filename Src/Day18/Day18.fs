module AdventOfCode23.Day18

open AdventOfCode23.Common
open System.Text.RegularExpressions

type BigVector = { X: bigint; Y: bigint; }

let main (lines: string list) =
    let char2Vec (c: char) =
        match c with
        | 'U' -> Vector.North
        | 'R' -> Vector.East
        | 'D' -> Vector.South
        | 'L' -> Vector.West
        | _ -> failwith "Oops..."

    let int2Vec (n: int) =
        match n with
        | 0 -> char2Vec 'R'
        | 1 -> char2Vec 'D'
        | 2 -> char2Vec 'L'
        | 3 -> char2Vec 'U'
        | _ -> failwith "Oops..."

    let findMin (vs: Vector list) =
        new Vector(vs |> List.map (fun v -> v.X) |> List.min, vs |> List.map (fun v -> v.Y) |> List.min)
    let findMax (vs: Vector list) =
        new Vector(vs |> List.map (fun v -> v.X) |> List.max, vs |> List.map (fun v -> v.Y) |> List.max)
    let findMinMax (vs: Vector list) = findMin vs, findMax vs

    let printMap (map: int array array) =
        for row in map do
            for col in row do
                printf "%c" (if col > 0 then '#' else '.')
            printfn ""

    let instructions = lines |> List.map (fun line ->
        let r = new Regex(@"(?<direction>[URDL]) (?<distance>\d+) \(#(?<color>[A-Za-z0-9]+)\)")
        let m = r.Match line
        if not m.Success then
            failwith "Oops..."
        (m.Groups.["direction"].Value.[0] |> char2Vec, m.Groups.["distance"].Value |> int, m.Groups.["color"].Value)
    )

    let floodFill = FloodFill((fun _ n -> n = 0), (fun _ _ -> 2))

    let makeTrenchPolygon (instructions: (Vector * int) list) =
        let rec loop (instructions: (Vector * int) list) (result: Vector list) =
            match instructions with
            | [] -> result
            | (direction, distance) :: tail -> loop tail <| result @ [ (List.last result) + (direction * distance) ]
        loop instructions [ Vector(0, 0) ]

    let makeTrenchMap (instructions: (Vector * int) list) =
        let trench = makeTrenchPolygon instructions
        let topLeft, bottomRight = findMinMax trench
        let trenchMap = Array.init (bottomRight.Y - topLeft.Y + 1) (fun _ ->
            Array.create (bottomRight.X - topLeft.X + 1) 0
        )
        for (v1, v2) in trench |> List.pairwise do
            let mn, mx = findMinMax [ v1 - topLeft; v2 - topLeft ]
            for y in [0..mx.Y - mn.Y] do
                for x in [0..mx.X - mn.X] do
                    trenchMap.[mn.Y + y].[mn.X + x] <- 1
        trenchMap

    let step1Instructions = instructions |> List.map (fun (v, d, _) -> (v, d))
    let trenchMap = makeTrenchMap step1Instructions
    let seed =
        let idx = trenchMap |> Array.collect id |> Array.findIndex (fun n -> n = 1)
        Vector(idx % trenchMap.Length, idx / trenchMap.Length) + Vector.East + Vector.South
    let floodedTrenchMap = trenchMap |> floodFill.Flood [ seed ]
    let area = floodedTrenchMap |> Array.collect id |> Array.filter (fun n -> n > 0) |> Array.length
    printfn "Cubic meters of lava the lagoon could hold: %d" area

    let step2Instructions = instructions |> List.map (fun (_, _, code) ->
        (code[code.Length - 1..] |> int |> int2Vec, System.Int32.Parse(code[..4], System.Globalization.NumberStyles.HexNumber))
    )
    let floodedPolygon = makeTrenchPolygon step2Instructions
    let perimeter =
        step2Instructions
        |> List.filter (fun (direction, _) -> direction = Vector.West || direction = Vector.North)
        |> List.map (fun (_, distance) -> distance |> bigint) |> ((@) [ 1I ])
        |> List.sum
    let area = 
        floodedPolygon |> List.map (fun v -> { X = v.X |> bigint; Y = v.Y |> bigint; }) |> List.pairwise
        |> List.map (fun (a, b) -> { X = a.X * b.Y; Y = a.Y * b.X; })
        |> List.fold (fun acc v -> { X = acc.X + v.X; Y = acc.Y + v.Y; }) { X = 0I; Y = 0I; }
        |> fun v -> (v.X - v.Y) / 2I
    printfn "Cubic meters of lava the lagoon could hold (using hex coordinates): %A" (perimeter + area)

