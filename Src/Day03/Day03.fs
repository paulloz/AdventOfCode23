module AdventOfCode23.Day03

open AdventOfCode23.Common
open System
open System.Text.RegularExpressions

let main (lines: string list) =
    let extractSeeds (isSeed: char -> bool) (board: char array array) =
        [ for i in 0 .. (board.Length - 1) do
            for j in 0 .. (board.[i].Length - 1) do
                if isSeed (board.[i].[j]) then
                    yield Vector(j, i) ]

    let r = new Regex(@"(\d+)")
    let sumAllNumbers (board: char array array) =
        board |> Seq.collect id |> String.Concat |> r.Matches |> Seq.sumBy (fun m -> m.Value |> int)

    let board = lines |> List.map (fun line -> line.ToCharArray()) |> Array.ofList
    let total = sumAllNumbers(board)

    let floodFill = FloodFill(
        (fun _ c -> c <> '.' && c <> ' '),
        (fun _ _ -> ' ')
    )

    let step1Seeds = board |> extractSeeds (fun c -> not (c = '.' || Char.IsDigit(c)))
    let flooded = floodFill.Flood step1Seeds board 
    let floodedTotal = sumAllNumbers(flooded)

    printfn "Sum of all parts: %d" (total - floodedTotal)

    // This is the most stupid, most ineficient way of doing step 2. But let's re-use the flood fill.
    let boardString = board |> Seq.collect id |> String.Concat
    let step2Seeds = board |> extractSeeds (fun c -> c = '*')
    let gearRatios = seq {
        for seed in step2Seeds do
            let mask = (floodFill.Flood [seed] board) |> Seq.collect id |> String.Concat
            let matches =
                seq { for i in 0..(mask.Length - 1) do if mask[i] = ' ' then boardString[i] else '.' }
                |> String.Concat |> r.Matches
            if matches.Count = 2 then
                matches |> Seq.map (fun m -> m.Value |> int) |> Seq.reduce (*)
            else
                0
        }
    printfn "Sum of all gear ratios: %d" (gearRatios |> Seq.sum)


