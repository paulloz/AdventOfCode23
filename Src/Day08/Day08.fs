module AdventOfCode23.Day08

open System.Text.RegularExpressions

let parseMap (lines: string list) =
    let r = new Regex(@"(?<key>\w+) = \((?<left>\w+), (?<right>\w+)\)", RegexOptions.Compiled)
    let makeTuple (left: string) (right: string) = [ ('L', left) ; ('R', right) ] |> Map.ofList
    lines
    |> List.map r.Match
    |> List.choose (fun (m: Match) ->
        if m.Success then
            Some((m.Groups.["key"].Value, makeTuple m.Groups.["left"].Value m.Groups.["right"].Value))
        else
            None
    )
    |> Map.ofList

let main (lines: string list) =
    let instructions = lines[0] |> List.ofSeq
    let map = lines.[2..] |> parseMap

    let distanceTo (predicate: string -> bool) (start: string) =
        let nextI (i: int) = (i + 1) % instructions.Length
        let rec loop (i: int) (path: string list) =
            let current = path.[path.Length - 1]
            if predicate current then
                path
            else
                loop (nextI i) (path @ [ map.[current].[instructions.[i]] ])
        [ 0; (loop 0 [start]).Length - 1 ] |> List.max

    let distanceToAll () =
        let rec gcd (a: uint64) (b: uint64) = if b = 0UL then a else gcd b (a % b)
        let rec lcm (l: uint64 list) =
            let lcm' (a: uint64) (b: uint64) = (a * b) / (gcd a b)
            match l with
            | [] -> 1UL
            | [ a; b ] -> lcm' a b
            | head :: tail -> lcm' head (lcm tail)
        let startingCells = map.Keys |> Seq.filter (fun (key: string) -> key.EndsWith('A'))
        let distances = startingCells |> Seq.map (distanceTo (fun (key: string) -> key.EndsWith('Z')))
        lcm (distances |> Seq.map (fun (x: int) -> x |> uint64) |> List.ofSeq)

    printfn "Number of steps required to reach ZZZ: %d" (distanceTo (fun (key: string) -> key = "ZZZ") "AAA")
    printfn "Number of steps required to reach **Z: %d" (distanceToAll ())
