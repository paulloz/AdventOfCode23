module AdventOfCode23.Day11

open System

let main (lines: string list) =
    let stars =
        lines |> List.mapi (fun y line ->
            line |> List.ofSeq |> List.mapi (fun x c ->
                if c = '#' then Some (x, y)
                else None
            )
        ) |> List.collect id |> List.choose id

    let expandedRows =
        lines |> List.map List.ofSeq
        |> List.mapi (fun i line -> if line |> List.forall ((=) '.') then Some i else None)
        |> List.choose id

    let expandedCols =
        lines |> List.map List.ofSeq |> List.transpose
        |> List.mapi (fun i line -> if line |> List.forall ((=) '.') then Some i else None)
        |> List.choose id

    let keyV (ax, ay) (bx, by) =
        if ax < bx then (ax, ay), (bx, by)
        else if ax > bx then (bx, by), (ax, ay)
        else if ay <= by then (ax, ay), (bx, by)
        else (bx, by), (ax, ay)

    let pairs =
        List.allPairs stars stars
        |> List.filter (fun (a, b) -> a <> b)
        |> List.map (fun (a, b) -> keyV a b)
        |> List.distinct

    let distances (expansionFactor: bigint) = 
        pairs |> List.map (fun ((ax, ay), (bx, by)) ->
            let addX = expandedCols |> List.filter (fun x -> x >= (min ax bx) && x < (max ax bx)) |> List.length |> bigint
            let addX = addX * expansionFactor
            let addY = expandedRows |> List.filter (fun y -> y >= (min ay by) && y < (max ay by)) |> List.length |> bigint
            let addY = addY * expansionFactor
            (Math.Abs(bx - ax) |> bigint) + (Math.Abs(by - ay) |> bigint) + addX + addY
        )

    printfn "Sum of distances between the stars: %A" (distances 1I |> List.sum)
    printfn "Sum of expanded distances between the stars: %A" (distances 999999I |> List.sum)
