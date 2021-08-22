module PDFRefl.Parse
open System
open System.Collections.Generic
open FParsec

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

let (.>>.>) (p: Parser<'a,'u>) (q: Lazy<Parser<'b,'u>>) =
    fun stream ->
        let reply1 = p stream
        if reply1.Status = Ok then
            let stateTag1 = stream.StateTag
            let reply2 = q.Force () stream
            let error = if stateTag1 <> stream.StateTag then reply2.Error
                        else mergeErrors reply1.Error reply2.Error
            let result = if reply2.Status = Ok then (reply1.Result, reply2.Result)
                         else Unchecked.defaultof<_>
            Reply(reply2.Status, result, error)
        else
            Reply(reply1.Status, reply1.Error)

type PDFObject =
    | PDFBoolean of bool
    | PDFNumber of decimal
    | PDFString of string
    | PDFName of string
    | PDFArray of PDFObject []
    | PDFDict of IDictionary<PDFObject, PDFObject>
    | PDFStream of IDictionary<PDFObject, PDFObject> * int64 * int64
    | PDFIndirect of uint64 * uint64 * PDFObject
    | PDFRef of uint64 * uint64
    | PDFNull

type PDFFormat = {
    Objects: PDFObject;
}

let ex_pfloat =
  tuple2 ((pstring "-." |>> fun _ -> "-0.") <|> (pstring "." |>> fun _ -> "0.")) (many digit)
  |>> fun (x, y) ->
      x :: List.map (fun z -> z.ToString()) y
      |> String.Concat
      |> Double.Parse

let pspcs =
    many (choice ([0; 9; 10; 12; 13; 32] |> List.map (fun i -> char i |> pchar)))
    |>> String.Concat

let ascii = new Text.ASCIIEncoding()
let ph (x : char) = Int32.Parse (x.ToString(), System.Globalization.NumberStyles.HexNumber)
let c2s c = [| byte c |] |> ascii.GetString
let pbool = (pstring "true" |>> fun _ -> PDFBoolean true) <|> (pstring "false" |>> fun _ -> PDFBoolean false)
let pnum = spaces >>. (attempt pfloat <|> ex_pfloat) |>> fun x -> PDFNumber (decimal x)
let poct = octal |>> fun x -> Int32.Parse (x.ToString ())
let pcode3 = pipe3 poct poct poct (fun a b c -> c + b * 8 + a * 8 * 8 |> c2s)
let pcode2 = pipe2 poct poct (fun a b -> b + a * 8 |> c2s)
let pcode1 = poct |>> c2s
let pcode = attempt pcode1 <|> attempt pcode2 <|> pcode3
let phex = (attempt (tuple2 (spaces >>. hex) (spaces >>. hex)) <|> (spaces >>. hex |>> fun h -> (h, '0')))
           |>> fun (a, b) -> c2s (16 * ph a + ph b)
let phexs = (pchar '<' >>. many1 phex .>> pchar '>') |>> String.Concat
let escaped = pchar '\\' >>. (pcode <|> (pchar 'n' |>> fun _ -> "\n") <|> (pchar 'r' |>> fun _ -> "r") <|> (pchar 't' |>> fun _ -> "\t") <|> (pchar 'b' |>> fun _ -> "\b") <|> (pchar 'f' |>> fun _ -> "\f") <|> (pchar '\\' |>> fun _ -> "\\") <|> (pchar '(' |>> fun _ -> ")") <|> (pchar ')' |>> fun _ -> ")"))
let en = (pchar '\\' >>. newline >>. spaces) |>> fun _ -> ""
let pchars = many (attempt (pstring "()") <|> attempt escaped <|> attempt en <|> attempt phexs <|> (noneOf ['('; ')'] |>> fun c -> c.ToString ()))
let pstr = (pchar '(' >>. spaces >>. pchars .>> spaces .>> pchar ')') |>> fun ss -> String.Concat ss |> PDFString // WIIIP
let pname = (spaces >>. pchar '/' >>. many (noneOf ([0; 9; 10; 12; 13; 32] |> List.map (fun i -> char i)))) |>> fun x -> List.fold (fun s c -> s + c.ToString ()) "" x |> PDFName // WIIIP
let pnull = pstring "null" |>> fun _ -> PDFNull
let pref = (tuple2 puint64 puint64 .>> pchar 'R') |>> PDFRef

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
    let q = manyCharsTill anyChar (followedBy (attempt (spaces >>. (pclose <|> popen))))
    let p = (popen .>> spaces) .>>. q |>> fun (a, b) -> itp (LParen a) (Str b)
    attempt (p .>>. (spaces >>. pclose) |>> fun (a, b) -> itp a (RParen b))
    <|>> lazy (tuple3 p (many (attempt (pspcs .>>. (iter popen pclose .>>. q |>> fun (a, b) -> itp a (Str b)) |>> fun (a, b) -> itp (Str a) b)) |>> IterSeq) (spaces >>. pclose) |>> fun (a, b, c) -> itp (itp a b) (RParen c))

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

let rec pobj () =
    attempt pref <|> attempt pbool <|> attempt pnum <|> attempt pstr <|> attempt pname <|> attempt pnull <|>> lazy (attempt (parr ())) <|>> lazy (attempt (pstream ())) <|>> lazy (attempt (pdict ())) <|>> lazy (pind ())
and parr () =
    myBetween (pstring "[") (pstring "]") (sepBy (spaces >>. pobj ()) (pchar ' ') |>> fun x -> List.toArray x |> PDFArray)
and pdict () =
    myBetween (pstring "<<") (pstring ">>") (sepBy (tuple2 (spaces >>. pname) (spaces >>. (pobj ()))) (pchar ' ') |>> fun x -> PDFDict (dict x))
and pind () =
    tuple3 puint64 puint64 (pstring "obj" >>. pobj () .>> pstring "endobj") |>> PDFIndirect
and pstream () = // WIIIP
    let rec iter () = attempt ((fun stream -> Reply stream.Index) .>> pstring "endstream") <|>> lazy (anyChar >>. iter ())
    tuple2 (pdict ()) (pstring "stream" >>. tuple2 (fun stream -> Reply stream.Index) (iter ()))
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
         | PDFDict dt -> PDFStream (dt, i, j)
         | _ -> raise (Exception ("never")))

let ParsePDF (pdf: IO.Stream) =
    runParserOnStream (pobj ()) () "" pdf System.Text.Encoding.ASCII
