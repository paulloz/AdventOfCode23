module AdventOfCode23.Day17

open System.Collections.Generic
open System.Linq

type Vector(x: int, y: int) =
    struct
        member this.X = x
        member this.Y = y

        member this.Inverse () = -this
        member this.Clockwise () =
            match this with
            | v when v = Vector.North -> Vector.East
            | v when v = Vector.East -> Vector.South
            | v when v = Vector.South -> Vector.West
            | v when v = Vector.West -> Vector.North
            | _ -> failwith "Oops..."
        member this.CounterClockwise () =
            match this with
            | v when v = Vector.North -> Vector.West
            | v when v = Vector.East -> Vector.North
            | v when v = Vector.South -> Vector.East
            | v when v = Vector.West -> Vector.South
            | _ -> failwith "Oops..."

        static member (*) (a: Vector, b: Vector) = Vector(a.X * b.X, a.Y * b.Y)
        static member (+) (a: Vector, b: Vector) = Vector(a.X + b.X, a.Y + b.Y)

        static member (*) (v: Vector, n: int) = Vector(v.X * n, v.Y * n)
        static member (*) (n: int, v: Vector) = Vector(v.X * n, v.Y * n)

        static member (~-) (v: Vector) = v * -1

        static member North = Vector(0, -1)
        static member East = Vector(1, 0)
        static member South = Vector(0, 1)
        static member West = Vector(-1, 0)

        override this.ToString() = sprintf "(%d;%d)" this.X this.Y
    end

type Node =
    {
        Position: Vector;
        Direction: Vector;
        Run: int;
    }

type ClosedKey = Vector * int
type ClosedMap = Map<ClosedKey, int>
type ClosedMaps = ClosedMap array array

let setHeat (p: Vector) (k: ClosedKey) (v: int) (cl: ClosedMaps) =
    let set (k: ClosedKey) (v: int) (cl: ClosedMap) =
        if Map.containsKey k cl then
            Map.remove k cl |> ignore
        Map.add k v cl
    Array.set cl.[p.Y] p.X <| set k v cl.[p.Y].[p.X]

let getHeat (p: Vector) (k: ClosedKey) (cl: ClosedMaps) = cl.[p.Y].[p.X].GetValueOrDefault(k)

let solve (nodes: int list list) (source: Vector) (destination: Vector) (minStep: int) (maxStep: int) =
    let isPositionValid (position: Vector) =
        position.Y >= 0 && position.Y < nodes.Length && position.X >= 0 && position.X < nodes.[position.Y].Length;

    let rec loop (queue: PriorityQueue<Node, int>) (closed: ClosedMap array array) =
        let rec advance (position: Vector) (direction: Vector) (run: int) (heat: int) =
            let rec loop' (heat: int) (i: int) =
                let nextPosition = position + (direction * i)
                match run + i with
                | run when run <= maxStep && isPositionValid nextPosition ->
                    let heat = heat + nodes.[nextPosition.Y].[nextPosition.X]
                    if i >= minStep then
                        let k = direction, run
                        match closed |> getHeat nextPosition k with
                        | heat' when heat' = 0 || heat' > heat ->
                            closed |> setHeat nextPosition k heat
                            queue.Enqueue({ Position = nextPosition; Direction = direction; Run = run; }, heat)
                        | _ -> ()
                    loop' heat (i + 1)
                | _ -> ()
            loop' heat 1

        if queue.Count <= 0 then
            closed.[destination.Y].[destination.X].Values.Min()
        else
            let current = queue.Dequeue()
            let heat = closed |> getHeat current.Position (current.Direction, current.Run)

            if current.Run < maxStep then
                advance current.Position current.Direction current.Run heat

            if current.Run >= minStep then
                advance current.Position (current.Direction.Clockwise()) 0 heat
                advance current.Position (current.Direction.CounterClockwise()) 0 heat

            loop queue closed

    let queue = PriorityQueue<Node, int>()
    queue.Enqueue({ Position = source; Direction = Vector.East; Run = 0; }, 0)
    queue.Enqueue({ Position = source; Direction = Vector.South; Run = 0; }, 0)

    loop queue (Array.init nodes.Length (fun y -> Array.create nodes.[y].Length (ClosedMap [])))

let main (lines: string list) =
    let nodes = lines |> List.map (fun line -> line |> List.ofSeq |> List.map System.Char.ToString |> List.map int)

    if nodes.Length <= 0 || (nodes |> List.map List.length |> List.distinct).Length > 1 then failwith "Oops..."

    let source = Vector(0, 0)
    let destination = Vector(nodes.[nodes.Length - 1].Length - 1, nodes.Length - 1)

    printfn "Least amount of heat loss: %d" (solve nodes source destination 1 3)
    printfn "Least amount of heat loss (4 <= steps <= 10): %d" (solve nodes source destination 4 10)
