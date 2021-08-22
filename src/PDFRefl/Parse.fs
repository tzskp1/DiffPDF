module PDFRefl.Parse
open System
open System.Collections.Generic
open FParsec

type PDFObject =
    | PDFBoolean of bool
    | PDFNumber of decimal
    | PDFString of string
    | PDFName of string
    | PDFArray of PDFObject []
    | PDFDict of IDictionary<PDFObject, PDFObject>
    | PDFStream of IDictionary<PDFObject, PDFObject> * IO.Stream
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

let pbool = (pstring "true" |>> fun _ -> PDFBoolean true) <|> (pstring "false" |>> fun _ -> PDFBoolean false)
let pnum = (attempt pfloat <|> ex_pfloat) |>> fun x -> PDFNumber (decimal x)
let pcode = pipe3 digit digit digit (fun a b c -> [| Int32.Parse (String.Format("{0}{1}{2}", a, b, c)) |> byte |] |> (new Text.ASCIIEncoding()).GetString)
let escaped = pchar '\\' >>. (pcode <|> (pchar 'n' |>> fun _ -> "\n") <|> (pchar 'r' |>> fun _ -> "r") <|> (pchar 't' |>> fun _ -> "\t") <|> (pchar 'b' |>> fun _ -> "\b") <|> (pchar 'f' |>> fun _ -> "\f") <|> (pchar '\\' |>> fun _ -> "\\") <|> (pchar '(' |>> fun _ -> ")") <|> (pchar ')' |>> fun _ -> ")"))
let pchars = many (attempt (pstring "()") <|> attempt escaped <|> (noneOf ['('; ')'] |>> fun c -> c.ToString ()))
let pstr = (pchar '(' >>. pchars .>> pchar ')') |>> fun ss -> String.Concat ss |> PDFString // WIIIP
let pname = (pchar '/' >>. many (noneOf ([0; 9; 10; 12; 13; 32] |> List.map (fun i -> char i)))) |>> fun x -> List.fold (fun s c -> s + c.ToString ()) "" x |> PDFName // WIIIP
let pnull = pstring "null" |>> fun _ -> PDFNull
let pref = (tuple2 puint64 puint64 .>> pchar 'R') |>> PDFRef

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

let rec pobj stm =
    attempt pref <|> attempt pbool <|> attempt pnum <|> attempt pstr <|> attempt pname <|> attempt pnull <|> attempt (pstream stm) <|> attempt (parr stm) <|> attempt (pdict stm) <|> attempt (pind stm)
and parr stm =
    pchar '[' >>. sepBy (pobj stm) spaces .>> pchar ']' |>> fun x -> List.toArray x |> PDFArray
and pdict stm =
    pstring "<<" >>. sepBy (tuple2 (spaces >>. pname) (spaces >>. pobj stm)) spaces .>> pstring ">>"
    |>> fun x -> PDFDict (dict x)
and pind stm =
    tuple3 puint64 puint64 (pstring "obj" >>. pobj stm .>> pstring "endobj") |>> PDFIndirect
and pstream (stm : IO.Stream) = // WIIIP
    let rec iter () = attempt ((fun stream -> Reply stream.Index) .>> pstring "endstream") <|>> lazy (anyChar >>. iter ())
    tuple2 (pdict stm) (pstring "stream" >>. tuple2 (fun stream -> Reply stream.Index) (iter ()))
    |>> (fun (dt, (i, j)) ->
         let _ = stm.Seek (i, IO.SeekOrigin.Begin)
         let stmn = new IO.MemoryStream ()
         let buffer = Array.create 32768 (byte 0)
         let bytes = ref (Math.Max (int (j - i), 0))
         let read = ref (stm.Read (buffer, 0, Math.Min (Array.length buffer, !bytes)))
         while !bytes > 0 && !read > 0 do
             stmn.Write (buffer, 0, !read)
             bytes := !bytes - !read
             read := stm.Read (buffer, 0, Math.Min(buffer.Length, !bytes))
         match dt with
         | PDFDict dt -> PDFStream (dt, stmn)
         | _ -> raise (Exception ("never")))

let ParsePDF (pdf: IO.Stream) =
    runParserOnStream (pobj pdf) () "" pdf System.Text.Encoding.ASCII
