module AdventOfCode23.Day09

let main (lines: string list) =
    let histories = lines |> List.map (fun (line: string) ->
        line.Split(" ") |> Seq.map int |> List.ofSeq
    )

    let solve (f: int list -> int -> int) =
        let computeNextValue (a: int, b: int) = b - a
        let rec loop (values: int list) =
            if (values |> List.sum) = 0 then
                0
            else
                f values (loop (values |> List.pairwise |> List.map computeNextValue))
        histories |> List.map loop |> List.sum

    let step1 (values: int list) (n: int) =
        values.[values.Length - 1] + n

    let step2 (values: int list) (n: int) =
        values.[0] - n

    printfn "Sum of extrapolated values: %d" (solve step1)
    printfn "Sum of backward extrapolated values: %d" (solve step2)
