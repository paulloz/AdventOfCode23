module AdventOfCode23.Day13

let main (lines: string list) =
    let findReflection (transpose: bool) (pattern: string list) =
        let pattern = pattern |> List.map List.ofSeq
        let pattern = if transpose then List.transpose pattern else pattern

        pattern
        |> List.mapi (fun i line -> i, line)
        |> List.choose (fun (i, line) ->
            let n = min i (pattern.Length - i)

            if i >= pattern.Length || n <= 0 then
                None
            else
                let a = pattern |> List.skip (i - n) |> List.truncate n
                let b = pattern |> List.skip i |> List.truncate n |> List.rev
                if a = b then Some(i) else None)

    let findReflections (pattern: string list) =
        findReflection true pattern, findReflection false pattern

    let patterns =
        (lines |> String.concat "|").Split("||")
        |> Seq.map (fun pattern -> pattern.Split("|") |> List.ofSeq)
        |> List.ofSeq

    let reflectionValues =
        patterns
        |> List.map findReflections
        |> List.map (fun (h, v) ->
            let h = h |> List.sum
            let v = v |> List.map ((*) 100) |> List.sum
            h + v)

    printfn "%d" (reflectionValues |> List.sum)
