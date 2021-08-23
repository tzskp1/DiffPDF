module PDFRefl.Format
open System

type PDFObject =
    | PDFBoolean of bool
    | PDFNumber of decimal
    | PDFString of string
    | PDFName of string
    | PDFArray of PDFObject []
    | PDFDict of Map<string, PDFObject>
    | PDFStream of StreamContent
    | PDFIndirect of uint64 * uint64 * PDFObject
    | PDFRef of uint64 * uint64
    | PDFNull
and StreamContent =
    { Dict: Map<string, PDFObject>; Start: int64; End: int64; Stream: option<IO.Stream> }

type PDFFormat = {
    Header: decimal;
    Objects: PDFObject;
}
