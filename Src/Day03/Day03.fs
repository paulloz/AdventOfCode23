module AdventOfCode23.Day03

open System
open System.Text.RegularExpressions

let extractSeeds (isSeed: char -> bool) (board: char array array) =
    [ for i in 0 .. (board.Length - 1) do
          for j in 0 .. (board.[i].Length - 1) do
              if isSeed (board.[i].[j]) then
                  yield (j, i) ]

let flood (inputBoard: char array array) (seeds: (int * int) list) =
    let board = inputBoard |> Array.map Array.copy

    let canFlood (x: int, y: int) =
        y >= 0 && y < board.Length && 
        x >= 0 && x < board.[y].Length &&
        board.[y].[x] <> '.' && board.[y].[x] <> ' '

    let rec loop = function
        | [] -> ()
        | flood ->
            let flood = flood |> List.distinct |> List.filter canFlood
            flood |> List.iter (fun (x: int, y: int) -> board.[y].[x] <- ' ')
            flood |> List.collect (fun (x: int, y: int) -> [
                (x - 1, y - 1); (x, y - 1); (x + 1, y - 1);
                (x - 1, y    );             (x + 1, y    );
                (x - 1, y + 1); (x, y + 1); (x + 1, y + 1)
            ]) |> loop

    for seed in seeds do
        loop [seed]

    board

let printBoard (board: char array array) =
    for line in board do
        for c in line do
            printf "%c" c
        printfn ""

let main (lines: string list) =
    let r = new Regex(@"(\d+)")
    let sumAllNumbers (board: char array array) =
        board |> Seq.collect id |> String.Concat |> r.Matches |> Seq.sumBy (fun m -> m.Value |> int)

    let board = lines |> List.map (fun line -> line.ToCharArray()) |> Array.ofList
    let total = sumAllNumbers(board)

    let step1Seeds = board |> extractSeeds (fun c -> not (c = '.' || Char.IsDigit(c)))

    let flooded = flood board step1Seeds
    let floodedTotal = sumAllNumbers(flooded)

    printfn "Sum of all parts: %d" (total - floodedTotal)
