module AdventOfCode23.Day16

type Cell =
    | Empty = 0
    | ForwardMirror = 1
    | BackwardMirror = 2
    | VerticalSplitter = 3
    | HorizontalSplitter = 4

type Direction =
    | East = 1
    | South = 2
    | West = 3
    | North = 4

let main (lines: string list) =
    let char2Cell (c: char) =
        match c with
        | '.' -> Cell.Empty
        | '/' -> Cell.ForwardMirror
        | '\\' -> Cell.BackwardMirror
        | '|' -> Cell.VerticalSplitter
        | '-' -> Cell.HorizontalSplitter
        | _ -> failwith <| sprintf "Invalid cell content '%c'" c

    let solve (map: Cell list list) (initial: (int * int * Direction)) =
        let isValidPosition (map: Cell list list) (x: int, y: int) = y >= 0 && y < map.Length && x >= 0 && x < map.[y].Length

        let isHorizontal (direction: Direction) = direction = Direction.East || direction = Direction.West
        let isVertical (direction: Direction) = direction = Direction.South || direction = Direction.North

        let alreadyVisited (cell: int) (fromDirection: Direction) = cell &&& (1 <<< (fromDirection |> int)) <> 0
        let visit (cell: int) (fromDirection: Direction) = cell ||| (1 <<< (fromDirection |> int))

        let rec loop (map: Cell list list) (result: int array array) (queue: (int * int * Direction) list) =
            match queue with
            | [] -> result
            | (x, y, direction) :: tail ->
                let (nextX, nextY) =
                    match direction with 
                    | Direction.East -> (x + 1, y)
                    | Direction.South -> (x, y + 1)
                    | Direction.West -> (x - 1, y)
                    | Direction.North -> (x, y - 1)
                    | direction -> failwith <| sprintf "Invalid direction '%A'" direction

                (
                    if isValidPosition map (nextX, nextY) && not <| alreadyVisited result.[nextY].[nextX] direction then
                        Array.set result.[nextY] nextX <| visit result.[nextY].[nextX] direction
                        match map.[nextY].[nextX] with
                        // Just go through.
                        | Cell.Empty -> [ (nextX, nextY, direction) ]
                        | Cell.VerticalSplitter when isVertical direction -> [ (nextX, nextY, direction) ]
                        | Cell.HorizontalSplitter when isHorizontal direction -> [ (nextX, nextY, direction) ]
                        // Split.
                        | Cell.VerticalSplitter ->  [ (nextX, nextY, Direction.South); (nextX, nextY, Direction.North) ]
                        | Cell.HorizontalSplitter ->  [ (nextX, nextY, Direction.West); (nextX, nextY, Direction.East) ]
                        // Turn.
                        | Cell.ForwardMirror when direction = Direction.East -> [ (nextX, nextY, Direction.North) ]
                        | Cell.ForwardMirror when direction = Direction.South -> [ (nextX, nextY, Direction.West) ]
                        | Cell.ForwardMirror when direction = Direction.West -> [ (nextX, nextY, Direction.South) ]
                        | Cell.ForwardMirror when direction = Direction.North -> [ (nextX, nextY, Direction.East) ]
                        | Cell.BackwardMirror when direction = Direction.East -> [ (nextX, nextY, Direction.South) ]
                        | Cell.BackwardMirror when direction = Direction.South -> [ (nextX, nextY, Direction.East) ]
                        | Cell.BackwardMirror when direction = Direction.West -> [ (nextX, nextY, Direction.North) ]
                        | Cell.BackwardMirror when direction = Direction.North -> [ (nextX, nextY, Direction.West) ]
                        // Oops.
                        | cell -> failwith <| sprintf "Invalid cell and direction (%A at (%d;%d))" direction nextX nextY
                    else
                        []
                ) @ tail |> loop map result

        loop map (Array.init map.Length (fun i -> Array.create map.[i].Length 0)) [initial]

    let countEnergizedCells (map: int array array) =
        map |> Array.collect id |> Array.fold (fun acc cell -> acc + (if cell > 0 then 1 else 0)) 0

    let map = lines |> List.map (fun line -> line |> Seq.map char2Cell |> List.ofSeq)

    if map.Length <= 0 || (map |> List.map List.length |> List.distinct).Length > 1 then
        failwith "Invalid map data"

    printfn "Number of energized tiles: %d" (solve map (-1, 0, Direction.East) |> countEnergizedCells)

    // Compute all the results for each edge.
    let east = map |> List.mapi (fun y _ -> solve map (-1, y, Direction.East) |> countEnergizedCells)
    let west = map |> List.mapi (fun y _ -> solve map (map.[0].Length, y, Direction.West) |> countEnergizedCells)
    let south = map.[0] |> List.mapi (fun x _ -> solve map (x, -1, Direction.South) |> countEnergizedCells)
    let north = map.[0] |> List.mapi (fun x _ -> solve map (x, map.Length, Direction.North) |> countEnergizedCells)
    // And keep the absolute best.
    printfn "Number of energized tiles in the best configuration: %d" (seq { east; west; south; north } |> Seq.collect id |> Seq.max)
