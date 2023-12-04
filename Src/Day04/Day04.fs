module AdventOfCode23.Day04

open System.Text.RegularExpressions;

let main (lines: string list) =
    let r = new Regex(@"(\d+)", RegexOptions.Compiled)

    let keepWinnings (winnings: Set<int>, numbers: Set<int>) = winnings |> Set.intersect numbers |> List.ofSeq

    let cards =
        lines
        |> List.map (fun line ->
            let line = line.Split(':').[1].Split('|')
            let extract numbers = numbers |> r.Matches |> Seq.map (fun m -> m.Value |> int) |> Set.ofSeq
            (line[0] |> extract, line[1] |> extract)
        )

    let scores =
        cards
        |> List.map keepWinnings
        |> List.map (fun winnings -> match winnings.Length with | 0 -> 0 | n -> 1 <<< n - 1)

    printfn "Total score: %d" (scores |> List.sum)

    let computeTotalNumberOfCards (cards: (Set<int> * Set<int>) list) =
        let outCards = [| for i in 1..cards.Length do yield 1 |]
        for i in 1..cards.Length do
            let winnings = (cards.[i - 1] |> keepWinnings).Length
            for j in (i + 1)..(i + winnings) do
                outCards.[j - 1] <- outCards.[j - 1] + outCards.[i - 1]
        outCards |> Seq.sum

    printfn "Total number of cards: %d" (cards |> computeTotalNumberOfCards)
