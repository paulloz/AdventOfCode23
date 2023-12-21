module AdventOfCode23.Day21

open AdventOfCode23.Common

type Tile =
    | Plot
    | Rock

let plotGarden (garden: Tile list list) (currentPosition: Vector) =
    for y in [0..garden.Length - 1] do
        for x in [0..garden.[y].Length - 1] do
            let c =
                if Vector(x, y) = currentPosition then "@"
                else match garden.[y].[x] with | Plot -> "." | Rock -> "#"
            printf "%s" c
        printfn ""

let plotDistances (garden: Tile list list) (distances: Map<Vector, int>) =
    for y in 0..garden.Length - 1 do
        for x in 0..garden.[y].Length - 1 do
            let c =
                match distances.TryFind (Vector(x, y)) with
                | Some distance -> sprintf "%02d " distance
                | None -> "   "
            printf "%s" c
        printfn ""

let main (lines: string list) =
    let garden, startPosition =
        let mutable start = Vector(-1, -1)
        let tiles = lines |> List.mapi (fun y line ->
            line |> Seq.mapi (fun x c ->
                if c = 'S' then start <- Vector(x, y)
                match c with
                | '.' -> Plot
                | '#' -> Rock
                | 'S' -> Plot
                | _ -> failwith "Oops..."
            ) |> List.ofSeq
        )
        tiles, start

    let solve (garden: Tile list list) (startPosition: Vector) =
        let propagate (v: Vector) =
            seq { Vector.North; Vector.East; Vector.South; Vector.West }
            |> Seq.map (fun v' -> v + v')
            |> Seq.filter (fun v' -> v'.Y >= 0 && v'.Y < garden.Length && v'.X >= 0 && v'.X < garden.[v'.Y].Length)
            |> Seq.filter (fun v' -> garden.[v'.Y].[v'.X] <> Rock)
            |> List.ofSeq
        let rec loop (result: Map<Vector, int>) (queue: (Vector * int) list) =
            match queue with
            | [] -> result
            | (position, distance) :: tail ->
                if garden.[position.Y].[position.X] = Rock || result.ContainsKey position then
                    loop result tail
                else
                    let result = result |> Map.add position distance
                    let propagation =
                        position |> propagate
                        |> List.filter (fun position -> position |> result.ContainsKey |> not)
                        |> List.map (fun position -> position, (distance + 1))
                    loop result (tail @ propagation)
        loop Map.empty<Vector, int> [ (startPosition, 0) ]

    let distances = solve garden startPosition
    printfn "Number of reachable garden plots: %d" (distances |> Map.values |> Seq.filter (fun d -> d <= 64 && d % 2 = 0 ) |> Seq.length)
