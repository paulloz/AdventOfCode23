namespace AdventOfCode23.Common

type FloodFill<'TCell>(canFlood: Vector -> 'TCell -> bool, doFlood: Vector -> 'TCell -> 'TCell) =
    member __.Flood (seeds: Vector list) (map: 'TCell array array) =
        let result = map |> Array.map Array.copy

        let propagate (v: Vector) = [
            v + Vector.West + Vector.North; v + Vector.North; v + Vector.East + Vector.North;
            v + Vector.West;                                  v + Vector.East;
            v + Vector.West + Vector.South; v + Vector.South; v + Vector.East + Vector.South;
        ]

        let rec loop (seeds: Vector list) =
            if seeds.Length > 0 then
                seeds |> List.distinct
                |> List.filter (fun v -> v.Y >= 0 && v.Y < result.Length && v.X >= 0 && v.X < result.[v.Y].Length)
                |> List.filter (fun v -> canFlood v result.[v.Y].[v.X])
                |> List.map (fun v ->
                    result.[v.Y].[v.X] <- doFlood v result.[v.Y].[v.X]
                    v
                ) |> List.collect propagate |> loop

        loop seeds
        result
