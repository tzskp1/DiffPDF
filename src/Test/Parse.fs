module PDFRefl.Parse
open FParsec
open Expecto
open PDFRefl.Parse

let testp p src ans =
    match run p src with
    | Success (res, _, _) ->
        Expect.equal res ans "These should equal"
    | _ -> Tests.failtest <| sprintf "failed to parse %s" src

[<Tests>]
let tests =
  testList "tests of parser" [
    testCase "number" <| fun _ ->
      testp pnum "+17" (PDFNumber (decimal 17))
      testp pnum "4." (PDFNumber (decimal 4.0))
      testp pnum "-.002" (PDFNumber (decimal -0.002))
    testCase "string" <| fun _ ->
      testp pstr "(This is a string)" (PDFString "This is a string")
      testp pstr "(These \\\n two strings \\\n are the same.)" (PDFString "These two strings are the same.")
      testp pstr "(These \\n two strings \\n are the same.)" (PDFString "These \n two strings \n are the same.")
  ]
