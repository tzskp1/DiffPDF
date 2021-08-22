// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Argu
open PDFRefl

type DiffPDFArg =
    | [<MainCommand>] Files of string list

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Files _ -> ""

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<DiffPDFArg>(programName = "diffpdf.exe")
    try
        let res = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let res = res.GetAllResults()
        match res with
        | [Files [a; b]] ->
            printfn "%A" (a, b)
        | _ ->
            raise <| System.FormatException "invalid format"
    with e ->
        printfn "%s" e.Message
    0 // return an integer exit code
