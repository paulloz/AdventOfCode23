module AdventOfCode23.Day14

let main (lines: string list) =
    let printMap (map: char list list) =
        for line in map do
            for c in line do
                printf "%c" c
            printfn ""

    let tilt (cells: char list) =
        let rec loop (result: char list) (cells: char list) =
            match cells with
            | [] -> result
            | 'O' :: tail -> loop (result @ [ 'O' ]) tail
            | '#' :: tail -> loop (result @ [ '#' ]) tail
            | '.' :: tail ->
                match (tail |> List.tryFindIndex ((=) 'O')), (tail |> List.tryFindIndex ((=) '#')) with
                | None, _ -> result @ [ '.' ] @ tail
                | Some i, Some j when j < i -> loop ( result @ [ '.' ] @ tail[..j]  ) tail[j + 1..]
                | Some i, _ -> loop (result @ [ 'O' ]) ([ for _ in [0..i] do yield '.' ] @ tail[i + 1..])
            | c :: _ -> failwith (c |> string)
        loop [] cells

    let tiltN (map: char list list) = map |> List.transpose |> List.map tilt |> List.transpose
    let tiltW (map: char list list) = map |> List.map tilt
    let tiltS (map: char list list) = map |> List.transpose |> List.map List.rev |> List.map tilt |> List.map List.rev |> List.transpose
    let tiltE (map: char list list) = map |> List.map List.rev |> List.map tilt |> List.map List.rev

    let spinCycle (map: char list list) = map |> tiltN |> tiltW |> tiltS |> tiltE

    let computeNorthBeamLoad (map: char list list) =
        map |> List.transpose |> List.map (fun col ->
            let length = List.length col
            col |> List.mapi (fun i c -> if c = 'O' then Some (length - i) else None) |> List.choose id
        ) |> List.collect id |> List.sum

    let map = lines |> List.map List.ofSeq

    printfn "Load on the north beam after one tilt: %d" (map |> tiltN |> computeNorthBeamLoad)

    let performNCycles (n: int) (map: char list list) =
        let rec loop (cycle: int) (cycleFrequency: int) (cache: Map<string, int>) (map: char list list) =
            if cycle >= n then
                map
            else
                let map = map |> spinCycle
                let cycle = cycle + 1
                if cycleFrequency > 0 then
                    // We already found a cycle, stay calm and walk through the last few iterations.
                    map |> loop cycle cycleFrequency cache
                else
                    // Trying to find a cycle and skip ahead.
                    let collapsed = map |> List.collect id |> System.String.Concat 
                    match cache |> Map.tryFind collapsed with
                    | None ->
                        // Didn't find anything, push the current value on the cache, and walk forward.
                        map |> loop cycle cycleFrequency (cache.Add (collapsed, cycle))
                    | Some foundIndex -> 
                        // We just found a valid cycle. With (n - cycles) iterations remaining, we can simply
                        // skip to (n - (remaining % cycleFrequency)), and walk from there.
                        let cycleFrequency = cycle - foundIndex
                        let cycle = n - ((n - cycle) % cycleFrequency)
                        map |> loop cycle cycleFrequency cache
        map |> loop 0 0 (Map [])

    let nCycles = 1_000_000_000
    printfn "Load on the north beam after %d cycles: %d" nCycles (map |> performNCycles nCycles |> computeNorthBeamLoad)
