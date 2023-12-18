module AdventOfCode23.Day01

open System.Text.RegularExpressions

let main (lines: string list) =
    let extractStep1CalibrationValue (line: string) =
        let r = new Regex(@"(\d)")
        let matches = r.Matches(line) |> Seq.map (fun m -> m.Groups[0].Value) |> Array.ofSeq
        if matches.Length > 0 then
            (matches[0] + matches[matches.Length - 1]) |> int
        else
            0

    let extractStep2CalibrationValue (line: string) =
        let r1 = new Regex(@"(one|two|three|four|five|six|seven|eight|nine|\d).*", RegexOptions.IgnoreCase)
        let r2 = new Regex(@".*(one|two|three|four|five|six|seven|eight|nine|\d)", RegexOptions.IgnoreCase)
        let mapExtractedValue (m: Match) =
            if m.Success then
                match m.Groups[1].Value with
                | "one" -> "1"
                | "two" -> "2"
                | "three" -> "3"
                | "four" -> "4"
                | "five" -> "5"
                | "six" -> "6"
                | "seven" -> "7"
                | "eight" -> "8"
                | "nine" -> "9"
                | (value: string) -> value
            else "0"
        let m1 = r1.Match(line) |> mapExtractedValue
        let m2 = r2.Match(line) |> mapExtractedValue
        (m1 + m2) |> int

    printfn "Calibration values sum (step 1): %d" (lines |> List.map extractStep1CalibrationValue |> List.sum)
    printfn "Calibration values sum (step 2): %d" (lines |> List.map extractStep2CalibrationValue |> List.sum)
