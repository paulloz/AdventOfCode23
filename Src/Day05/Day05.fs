module AdventOfCode23.Day05

open System.Text.RegularExpressions

let main (lines: string list) =
    let lineToInts (line: string) =
        line.Split ' ' |> Seq.choose (fun str -> if str.Length > 0 then Some(str |> uint) else None) |> List.ofSeq

    let isInRange (start: uint) (length: uint) (n: uint) = n >= start && n < start + length

    let r = new Regex("(\w+)-to-(\w+) map:")

    let lines = (lines |> Array.ofList |> String.concat "\n").Split "\n\n"

    // Find all our initial seeds.
    let seeds = (lines.[0].Split ':').[1] |> lineToInts
    // Build all the maps.
    let maps =
        lines.[1..]
        |> Seq.map (fun section -> section.Split "\n")
        |> Seq.map (fun lines ->
            let mapDescription = r.Match lines.[0]
            let mapping =
                lines.[1..]
                |> Seq.map lineToInts
                |> Seq.map (fun ranges -> (ranges[1], ranges[0], ranges[2]))
                |> List.ofSeq
            (mapDescription.Groups.[1].Value, (mapDescription.Groups.[2].Value, mapping))
        )
        |> Map.ofSeq

    // Dumb and straightforward, follow each seed down the line.
    // In the end, keep the smallest.
    let reduce (value: uint) =
        let rec loop (key: string) (value: uint) =
            match maps.TryFind key with
            // General case.
            | Some(output, mapping) ->
                match mapping |> Seq.tryPick (fun (input, output, length) ->
                    if isInRange input length value then
                        Some(output + (value - input))
                    else
                        None
                ) with
                | None -> value
                | Some(value) -> value
                // Recurse down a level.
                |> loop output
            // This is our exit.
            | None -> value
        loop "seed" value

    let locations = seeds |> List.map reduce
    printfn "Closest location: %u" (locations |> List.min)

    // Find all our original seed ranges.
    let seeds =
        seeds
        |> List.pairwise
        |> List.mapi (fun idx values -> idx, values)
        |> List.choose (fun (idx, tpl) -> if idx % 2 = 0 then Some(tpl) else None)
    // Build all the maps, simply reversing the old ones.
    let maps =
        maps
        |> Map.toSeq
        |> Seq.map (fun (input, (output, mapping)) -> (output, (input, mapping)))
        |> Map.ofSeq

    // Still dumb, but we follow any possible location up the line.
    // We pick the first one mapping to a valid seed.
    let reduce2 (value: uint) =
        let rec loop (key: string) (value: uint) =
            match maps.TryFind key with
            // General case.
            | Some(input, mapping) ->
                match mapping |> Seq.tryPick (fun (input, output, length) ->
                    if isInRange output length value then
                        Some(input + (value - output))
                    else
                        None
                ) with
                | None -> value
                | Some(value) -> value
                // Recurse up a level.
                |> loop input
            // This is our exit.
            | None when key = "seed" ->
                match seeds |> List.tryFind (fun (start, length) -> isInRange start length value) with
                // The seed actually exists in one of our valid ranges, return it.
                | Some(start, length) -> Some(value)
                // The seed is nowhere to be found in our valid ranges, return None.
                | None -> None
            // This should never happen.
            | None -> failwith "Oops."
        loop "location" value

    let (location, seed) =
        seq { for value in 0u..System.UInt32.MaxValue do yield value, reduce2 value }
        |> Seq.find (fun (i, opt) -> opt.IsSome)
    if seed.IsNone then
        failwith "Oops."
    printfn "Close location (with seed ranges): %u" location
