module PDFRefl.Parse
open FParsec
open Expecto
open PDFRefl.Format
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
      testp pstr "(This %is a string)" (PDFString "This %is a string")
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
    testCase "dict" <| fun _ ->
      testp (pobj ()) "<< /Type /Example /Subtype /DictionaryExample /Version 0.01 /IntegerItem 12 /StringItem ( a string ) /Subdictionary << /Item1 0.4 /Item2 true /LastItem ( not! ) /VeryLastItem ( OK ) >> >>"
            (PDFDict <| Map ["Type", PDFName "Example";
                              "Subtype", PDFName "DictionaryExample";
                              "Version", PDFNumber 0.01M;
                              "IntegerItem", PDFNumber 12M;
                              "StringItem", PDFString " a string ";
                              "Subdictionary", PDFDict <| Map ["Item1", PDFNumber 0.4M; "Item2", PDFBoolean true; "LastItem", PDFString " not! "; "VeryLastItem", PDFString " OK "]])
    testCase "indirect" <| fun _ ->
      testp (pobj ()) "12 0 obj\n (Brillig) \nendobj" (PDFIndirect (12UL, 0UL, PDFString "Brillig"))
    testCase "ref" <| fun _ ->
      testp (pobj ()) "12 0 R" (PDFRef (12UL, 0UL))
    testCase "stream" <| fun _ ->
      testp (pobj ()) "7 0 obj\n << /Length 8 0 R >>\n stream\n BT\n /F1 12 Tf\n 72 712 Td\n ( A stream with an indirect length ) Tj\n ET\n endstream\n endobj"
            (PDFIndirect (7UL, 0UL, PDFStream { Dict=Map ["Length", PDFRef (8UL, 0UL)]; Start=36L; End=109L; Stream=None }))
  ]
