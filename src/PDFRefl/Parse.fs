module PDFRefl.Parse
open System
open System.Collections.Generic
open FParsec
open PDFRefl.Format

let (<|>>) (p1: Parser<'a,'u>) (p2: Lazy<Parser<'a,'u>>) : Parser<'a,'u> =
    fun stream ->
        let mutable stateTag = stream.StateTag
        let mutable reply = p1 stream
        if reply.Status = Error && stateTag = stream.StateTag then
            let error = reply.Error
            reply <- p2.Force () stream
            if stateTag = stream.StateTag then
                reply.Error <- mergeErrors reply.Error error
        reply

let ex_pfloat =
  tuple2 ((pstring "-." |>> fun _ -> "-0.") <|> (pstring "." |>> fun _ -> "0.")) (many digit)
  |>> fun (x, y) ->
      x :: List.map (fun z -> z.ToString()) y
      |> String.Concat
      |> Double.Parse

let spcs_chars = [0; 9; 10; 12; 13; 32] |> List.map char

let comment =
    pchar '%' >>. manyCharsTill anyChar newline >>. preturn ' '

let pspcs =
    many (choice (comment :: (spcs_chars |> List.map pchar)))
    |>> String.Concat

type iterTy =
    | LParen of string
    | RParen of string
    | Str of string
    | IterSeq of list<iterTy>

let itp a b =
    match a, b with
    | IterSeq as_, IterSeq bs ->
        List.concat [as_; bs] |> IterSeq
    | _, IterSeq bs ->
        a :: bs |> IterSeq
    | IterSeq as_, b ->
        List.append as_ [b] |> IterSeq
    | _, _ ->
        IterSeq [a; b]

let rec iter popen pclose =
    let q = manyCharsTill anyChar (followedBy (attempt (pspcs >>. (pclose <|> popen))))
    let p = (popen .>> pspcs) .>>. q |>> fun (a, b) -> itp (LParen a) (Str b)
    attempt (p .>>. (pspcs >>. pclose) |>> fun (a, b) -> itp a (RParen b))
    <|>> lazy (tuple3 p (many (attempt (pspcs .>>. (iter popen pclose .>>. q |>> fun (a, b) -> itp a (Str b)) |>> fun (a, b) -> itp (Str a) b)) |>> IterSeq) (pspcs >>. pclose) |>> fun (a, b, c) -> itp (itp a b) (RParen c))

let rec iter_flatten a =
    match a with
    | Str sa | LParen sa | RParen sa -> sa
    | IterSeq as_ ->
        List.map iter_flatten as_
        |> List.fold (fun a b -> a + b) ""

let eliminate_parens a =
    let rec iter = function
    | [] -> []
    | [RParen _] -> []
    | x :: xs -> x :: iter xs
    match a with
    | IterSeq (LParen _ :: as_) ->
        iter as_ |> IterSeq |> iter_flatten
    | _ -> failwith "never: eliminate_parens"

let myBetween popen pclose p stream =
    let r = stream |> (iter popen pclose |>> fun s -> run p (eliminate_parens s))
    if r.Status = Ok then
        match r.Result with
        | Success (res, (), pos) ->
            Reply (r.Status, res, r.Error)
        | Failure _ ->
            Reply (Error, r.Error)
    else Reply (r.Status, r.Error)

let index: Parser<int64, unit> = fun stream -> Reply stream.Index
let ascii = new Text.ASCIIEncoding()
let ph (x : char) = Int32.Parse (x.ToString(), System.Globalization.NumberStyles.HexNumber)
let c2s c = [| byte c |] |> ascii.GetString
let pbool = (pstring "true" |>> fun _ -> PDFBoolean true) <|> (pstring "false" |>> fun _ -> PDFBoolean false)
let pnum = pspcs >>. (attempt pfloat <|> ex_pfloat) |>> fun x -> PDFNumber (decimal x)
let poct = octal |>> fun x -> Int32.Parse (x.ToString ())
let pcode3 = pipe3 poct poct poct (fun a b c -> c + b * 8 + a * 8 * 8 |> c2s)
let pcode2 = pipe2 poct poct (fun a b -> b + a * 8 |> c2s)
let pcode1 = poct |>> c2s
let pcode = attempt pcode1 <|> attempt pcode2 <|> pcode3
let phex = (attempt (tuple2 (pspcs >>. hex) (pspcs >>. hex)) <|> (pspcs >>. hex |>> fun h -> (h, '0')))
           |>> fun (a, b) -> c2s (16 * ph a + ph b)
let phexs = pchar '<' >>. many1 phex .>> pchar '>'
let escaped = pchar '\\' >>. (pcode <|> (pchar 'n' |>> fun _ -> "\n") <|> (pchar 'r' |>> fun _ -> "r") <|> (pchar 't' |>> fun _ -> "\t") <|> (pchar 'b' |>> fun _ -> "\b") <|> (pchar 'f' |>> fun _ -> "\f") <|> (pchar '\\' |>> fun _ -> "\\") <|> (pchar '(' |>> fun _ -> ")") <|> (pchar ')' |>> fun _ -> ")"))
let en = (pchar '\\' >>. newline >>. pspcs) |>> fun _ -> ""
let pchars = many (attempt (pstring "()") <|> attempt escaped <|> attempt en <|> (noneOf ['('; ')'] |>> fun c -> c.ToString ()))
let pstr = (pchar '(' >>. pchars .>> pchar ')' <|> attempt phexs) |>> fun ss -> String.Concat ss |> PDFString // WIIIP
let pname = (pspcs >>. pchar '/' >>. many (noneOf spcs_chars)) |>> fun x -> List.fold (fun s c -> s + c.ToString ()) "" x |> PDFName // WIIIP
let pnull = pstring "null" |>> fun _ -> PDFNull
let pref = (tuple2 puint64 (pspcs >>. puint64) .>> pspcs .>> pchar 'R') |>> PDFRef

let rec pobj () =
    attempt pbool <|> attempt pnull <|>> lazy (attempt (parr ())) <|>> lazy (attempt (pstream ())) <|>> lazy (attempt (pdict ())) <|>> lazy (attempt (pind ())) <|> attempt pref <|> attempt pnum <|> attempt pstr <|> attempt pname
and parr () =
    myBetween (pstring "[") (pstring "]") (many (pspcs >>. pobj ()) |>> fun x -> List.toArray x |> PDFArray)
and pdict () =
    myBetween (pstring "<<") (pstring ">>") (many (tuple2 (pspcs >>. pname |>> function PDFName x -> x | _ -> failwith "never") (pspcs >>. (pobj ()))) |>> fun x -> PDFDict (Map x))
and pind () =
    tuple3 puint64 (pspcs >>. puint64) (pspcs >>. pstring "obj" >>. pspcs >>. pobj () .>> pspcs .>> pstring "endobj") |>> PDFIndirect
and pstream () = // WIIIP
    let rec iter () = attempt (index .>> pstring "endstream") <|>> lazy (anyChar >>. iter ())
    tuple2 (pdict () .>> pspcs) (pstring "stream" >>. tuple2 index (iter ()))
    |>> (fun (dt, (i, j)) ->
         // let _ = stm.Seek (i, IO.SeekOrigin.Begin)
         // let stmn = new IO.MemoryStream ()
         // let buffer = Array.create 32768 (byte 0)
         // let bytes = ref (Math.Max (int (j - i), 0))
         // let read = ref (stm.Read (buffer, 0, Math.Min (Array.length buffer, !bytes)))
         // while !bytes > 0 && !read > 0 do
         //     stmn.Write (buffer, 0, !read)
         //     bytes := !bytes - !read
         //     read := stm.Read (buffer, 0, Math.Min(buffer.Length, !bytes))
         match dt with
         | PDFDict dt -> PDFStream { Dict=dt; Start=i; End=j; Stream=None }
         | _ -> raise (Exception ("never")))

let ParsePDF (pdf: IO.Stream) =
    runParserOnStream (pobj ()) () "" pdf System.Text.Encoding.ASCII
