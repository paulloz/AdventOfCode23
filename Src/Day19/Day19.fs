module AdventOfCode23.Day19

open System.Text.RegularExpressions

let main (lines: string list) =
    let (|ParseRegex|_|) regex str =
        match Regex(regex).Match str with
        | m when m.Success -> Some (List.tail [ for x in m.Groups -> x.Value ])
        | _ -> None

    let (|Integer|_|) str =
        try
            str |> int |> Some
        with :? System.FormatException -> None

    let (workflowLines, partLines) =
        let idx = lines |> List.findIndex (fun line -> line.Length = 0)
        lines[..idx - 1], lines[idx + 1..]

    let workflows =
        workflowLines |> List.choose (fun workflowLine ->
            match workflowLine with
            | ParseRegex @"(.+){(.+)}" [ name; rules ] ->
                Some (name, rules.Split(",") |> Array.map (fun rule ->
                    match rule with
                    | ParseRegex @"(.+)([<>])(\d+):(.+)" [ k; op; Integer v; out ] ->
                        let f: Map<string, int> -> bool =
                            match op with
                            | ">" -> fun part -> part.[k] > v
                            | "<" -> fun part -> part.[k] < v
                            | _ -> failwith <| sprintf "Oops... Unknown operation '%s'" op
                        f, out
                    | out -> (fun _ -> true), out
                ) |> List.ofArray)
            | _ -> None
        ) |> Map.ofList

    let parts =
        partLines |> List.choose (fun partLine ->
            match partLine with
            | ParseRegex @"{(.+)}" [ partLine ] ->
                Some (partLine.Split(",") |> Array.choose (fun keyValue ->
                    match keyValue with
                    | ParseRegex @"(.+)=(\d+)" [ k; Integer v ] -> Some (k, v)
                    | _ -> None
                ) |> Map.ofArray)
            | _ -> None
        )

    let processPart (part: Map<string, int>) =
        let rec runWorkflow (workflowKey: string) =
            let rec loop (workflow: ((Map<string, int> -> bool) * string) list) =
                match workflow with
                | [] -> "R"
                | (f, out) :: tail -> if f part then out else loop tail
            if workflows |> Map.containsKey workflowKey then
                match loop workflows.[workflowKey] with
                | "R" -> "R"
                | "A" -> "A"
                | workflowKey -> runWorkflow workflowKey
            else "R"
        runWorkflow "in"

    let acceptedParts = parts |> List.filter (fun part -> (processPart part) = "A")
    printfn "Sum of all accepted ratings: %d" (acceptedParts |> List.map (fun part -> part |> Map.values |> Seq.sum) |> List.sum)

