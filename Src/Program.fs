module AdventOfCode23.Main

open Argu
open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions

let ProgramName = "adventofcode23"

[<CliPrefix(CliPrefix.DoubleDash)>]
type Arguments =
    | [<Unique; AltCommandLineAttribute("-d")>] Day of uint
    | [<ExactlyOnce; AltCommandLine("-i"); MainCommand>] Input of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Day _ -> "what day to run."
            | Input _ -> "the .txt file containing day input."

[<EntryPoint>]
let main argv =
    let errorHandler =
        ProcessExiter(
            colorizer =
                function
                | ErrorCode.HelpText -> None
                | _ -> Some ConsoleColor.Red
        )

    let parser =
        ArgumentParser.Create<Arguments>(programName = ProgramName, errorHandler = errorHandler)

    let parserResults = parser.ParseCommandLine(argv)

    let r = new Regex(@"^AdventOfCode23.Day(\d+)$", RegexOptions.Compiled)

    let days =
        (Assembly.GetExecutingAssembly().GetTypes()
         |> Seq.choose (fun t ->
             let m = r.Match(t.FullName)
             let method = t.GetMethod("main")

             if m.Success && method <> null then
                 Some((m.Groups[1].Value |> uint, method))
             else
                 None)
         |> Map.ofSeq)

    if days.Count <= 0 then
        parser.ErrorHandler.Exit(parser.PrintUsage(), ErrorCode.HelpText)

    let day = parserResults.GetResult<uint>(Day, days |> Map.keys |> Seq.max)
    let input = parserResults.GetResult(Input, "")

    if not (days.ContainsKey(day)) then
        parser.ErrorHandler.Exit(
            $"ERROR: Invalid parameter '{day}' for '<day>'.\n{parser.PrintUsage()}",
            ErrorCode.CommandLine
        )

    try
        days[day].Invoke(null, [| File.ReadAllLines(input) |> List.ofArray |]) |> ignore
    with :? TargetInvocationException as e ->
        raise e.InnerException

    0
