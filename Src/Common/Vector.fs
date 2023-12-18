namespace AdventOfCode23.Common

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
        static member (-) (a: Vector, b: Vector) = a + (-b)

        static member (*) (v: Vector, n: int) = Vector(v.X * n, v.Y * n)
        static member (*) (n: int, v: Vector) = Vector(v.X * n, v.Y * n)

        static member (~-) (v: Vector) = v * -1

        static member North = Vector(0, -1)
        static member East = Vector(1, 0)
        static member South = Vector(0, 1)
        static member West = Vector(-1, 0)

        override this.ToString() = sprintf "(%d;%d)" this.X this.Y
    end
