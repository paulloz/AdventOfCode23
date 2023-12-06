module AdventOfCode23.Day06

open System.Text.RegularExpressions

let extractData (lines: string list) =
    let r = new Regex(@"(\d+)", RegexOptions.Compiled)
    let extractNumbers (line: string) =
        line.Split(':')[1] |> r.Matches |> Seq.map (fun m -> m.Value |> uint64)
    (extractNumbers lines.[1]) |> Seq.zip (extractNumbers lines.[0]) |> List.ofSeq

let findSolutions (data: (uint64 * uint64) list) =
    let findSolution (time: uint64, distance: uint64) =
        let solution (t: uint64) = (time - t) * t > distance
        let min = seq { 0UL..time } |> Seq.find solution
        let max = seq { 0UL..time } |> Seq.findBack solution
        min, max
    data |> List.map findSolution

let rangeLength (min: uint64, max: uint64) =
    if min = max then
        0UL
    else
        max - min + 1UL

let main (lines: string list) =
    let r = new Regex(@"(\d+)")

    let data = lines |> extractData
    let solutions = data |> findSolutions
    printfn "Step 1 solution: %d" (solutions |> List.map rangeLength |> List.reduce (*))

    let data = lines |> List.map (fun line -> line.Replace(" ", "")) |> extractData
    let solutions = data |> findSolutions
    printfn "Step 2 solution: %d" (solutions |> List.map rangeLength |> List.reduce (*))
