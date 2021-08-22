module PDFRefl.Parse
open FParsec
open Expecto
open PDFRefl.Parse

let testp p src ans =
    match run p src with
    | Success (res, _, _) ->
        Expect.equal res ans "These should equal"
    | Failure (err, _, s) ->
        Tests.failtest <| sprintf "failed to parse %s. err: %s state : %A" src err s

[<Tests>]
let tests =
  testList "tests of parser" [
    testCase "number" <| fun _ ->
      testp pnum "+17" (PDFNumber 17M)
      testp pnum "4." (PDFNumber 4.0M)
      testp pnum "-.002" (PDFNumber -0.002M)
    testCase "string" <| fun _ ->
      testp pstr "(This is a string)" (PDFString "This is a string")
      testp pstr "(These \\\n two strings \\\n are the same.)" (PDFString "These two strings are the same.")
      testp pstr "(These \\n two strings \\n are the same.)" (PDFString "These \n two strings \n are the same.")
    testCase "name" <| fun _ ->
      testp pname "/SomeName" (PDFName "SomeName")
      testp pname "/SomeName " (PDFName "SomeName")
      testp pname " /SomeName" (PDFName "SomeName")
      testp pname " /SomeName " (PDFName "SomeName")
    testCase "array" <| fun _ ->
      testp (pobj ()) "[549 3.14 false (Ralph) /SomeName]" (PDFArray [| PDFNumber (decimal 549); PDFNumber (decimal 3.14); PDFBoolean false; PDFString "Ralph"; PDFName "SomeName" |])
      testp (pobj ()) "[ 549 3.14 false (Ralph) /SomeName ]" (PDFArray [| PDFNumber (decimal 549); PDFNumber (decimal 3.14); PDFBoolean false; PDFString "Ralph"; PDFName "SomeName" |])
      testp (manyCharsTill anyChar (pchar '[' <|> pchar ']') .>>. manyCharsTill anyChar (pchar '[' <|> pchar ']')) "[ 549 3.14 false (Ralph) /SomeName ]" ("", "")
    // testCase "dict" <| fun _ ->
    //   testp (pobj ()) "<< /Type /Example /Subtype /DictionaryExample /Version 0.01 /IntegerItem 12 /StringItem ( a string ) /Subdictionary << /Item1 0.4 /Item2 true /LastItem ( not! ) /VeryLastItem ( OK ) >> >>" PDFNull
  ]
