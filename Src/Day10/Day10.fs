module AdventOfCode23.Day10

let main (lines: string list) =
    let mapmap (f: (int * int) -> char -> 'a) (map: char list list) =
        map |> List.mapi (fun y line -> line |> List.mapi (fun x c -> f (x, y) c))

    let printMap (map: char list list) =
        for line in map do
            for c in line do
                let c  =
                    match c with
                    | '-' -> '─'
                    | '|' -> '│'
                    | 'F' -> '╭'
                    | '7' -> '╮'
                    | 'J' -> '╯' 
                    | 'L' -> '╰'
                    | 'S' -> 'O'
                    | '.' -> ' '
                    | c -> c
                printf "%c" c
            printfn ""

    let printMapAndCells (map: char list list) (cells: (int * int) list) (pipeLoop: Option<(int * int) list>) =
        map |> mapmap (fun (x, y) c ->
            if List.contains (x, y) cells then '#'
            else if pipeLoop.IsNone || List.contains (x, y) pipeLoop.Value then c
            else '.'
        ) |> printMap

    let findPipeLoop (map: char list list) =
        let getNeighbourCells (x: int) (y: int) =
            match map.[y].[x] with
            | '-' -> [(-1, 0); (1, 0)]
            | '|' -> [(0, -1); (0, 1)]
            | 'F' -> [(1, 0); (0, 1)]
            | '7' -> [(-1, 0); (0, 1)]
            | 'J' -> [(-1, 0); (0, -1)]
            | 'L' -> [(1, 0); (0, -1)]
            | 'S' -> [(1, 0); (0, 1); (-1, 0); (0, -1)]
            | _ -> []
            |> List.map (fun (x', y') -> x + x', y + y')
            |> List.filter (fun (x', y') -> y' >= 0 && y' < map.Length && x' >= 0 && x' < map.[y'].Length)

        let rec loop (n: int) (visited: (int * int) list) (x: int, y: int) =
            // Find all the neighbour cells connected to this pipe.
            let neighbours = (x, y) ||> getNeighbourCells |> List.except [ visited.[visited.Length - 1] ]

            // Finally found the exit (a.k.a. the first visited cell), exit recursion.
            if List.contains visited.Head neighbours then
                Some <| visited @ [ (x, y) ]
            else
                // Only keep neighbours that are pipes connected to us (and us connected to them).
                let validNeighbours =
                    neighbours
                    |> List.filter (fun (x', y') -> Array.contains map.[y'].[x'] [|'-';'|';'F';'7';'J';'L';'S'|])
                    |> List.filter (fun (x', y') -> List.contains (x, y) <| getNeighbourCells x' y')
                match validNeighbours with
                // Nowhere to follow, exit recursion.
                | [] -> None
                // One possible path, valid loop, continue.
                | [neighbour] -> loop (n + 1) (visited @ [ (x, y) ]) neighbour
                // Too many possible paths, not on a valid loop, exit recursion.
                | _ -> None

        let start =
            map |> List.mapi (fun y line ->
                match line |> List.tryFindIndex ((=) 'S') with
                | Some(x) -> Some (x, y)
                | None -> None
            ) |> List.choose id |> List.head

        start ||> getNeighbourCells |> List.choose (loop 1 [ start ]) |> List.maxBy List.length

    let findCells (map: char list list) (pipeLoop: (int * int) list) =
        map |> mapmap (fun (x, y) c ->
            if List.contains (x, y) pipeLoop then
                None
            else
                // Cast a line from the current cell to (0, 0). If the cell is inside the loop, it'll intersect
                // the polygon an odd number of times. If not, it'll intersect an even one.
                let nIntersects =
                    [0..(min x y)] |> Seq.map (fun i -> x - i, y - i)
                    |> Seq.filter (fun cell -> List.contains cell pipeLoop)
                    |> Seq.filter (fun (x', y') -> not <| Array.contains map.[y'].[x'] [|'L';'7'|])
                    |> Seq.length
                if nIntersects % 2 = 0 then None else Some (x, y)
        ) |> List.collect id |> List.choose id

    let map = lines |> List.map List.ofSeq

    let pipeLoop = findPipeLoop map

    printMap map
    printfn "Furthest distance from the loop entry: %d" (pipeLoop.Length / 2)

    let cells = findCells map pipeLoop

    printMapAndCells map cells (Some pipeLoop)
    printfn "Number of enclosed cells: %A" cells.Length
