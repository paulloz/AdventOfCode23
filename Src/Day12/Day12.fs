module AdventOfCode23.Day12

let main (lines: string list) =
    let rec verifyLine (actual: char list) (expected: int list) =
        match expected with
        | _ when actual.Length > 0 && actual.[0] = '.' -> verifyLine (actual |> List.skipWhile ((=) '.')) expected
        | head :: tail when (actual |> List.takeWhile ((=) '#')).Length = head -> verifyLine (actual |> List.skip head) tail
        | [] when actual |> List.forall ((=) '.') -> true
        | _ -> false

    let solveLine (input: char list) (expected: int list) =
        let rec loop (solve: int) (current: char list) (input: char list) (expected: int list) =
            match input with
            | [] when verifyLine current expected -> solve + 1
            | [] -> solve
            | head :: tail when head <> '?' -> loop solve (current @ [ head ]) tail expected
            | _ :: tail ->
                let a = loop solve (current @ [ '.' ]) tail expected
                let b = loop solve (current @ [ '#' ]) tail expected
                solve + a + b

        loop 0 [] input expected

    let data =
        lines
        |> List.map (fun line ->
            let line = line.Split(' ')
            (line.[0] |> List.ofSeq), (line.[1].Split(',') |> Array.map int |> List.ofArray))

    printfn "Total possibilities: %d" (data |> List.map (fun line -> solveLine <|| line) |> List.sum)
