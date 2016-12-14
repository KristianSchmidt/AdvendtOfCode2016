#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"

open System
open System.IO
open FParsec
open FParsec.Primitives
open FParsec.CharParsers
open FParsec.Error

fsi.PrintWidth <- 180

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "day9.txt")

type StringPart =
    | Repeat of n : Int64 * s : StringPart List
    | Normal of string

let parseRepeat : Parser<_,unit> =
    let parseParens =
        let parseIntxInt = tuple2 (pint32 .>> pstring "x") pint32
        pstring "(" >>. parseIntxInt .>> pstring ")"
    parseParens >>= (fun (l,r) -> manyMinMaxSatisfy 0 l (fun _ -> true) |>> (fun s -> Repeat (int64 r, [Normal s])))

let parseNormalPart : Parser<_,unit> = many1Satisfy (fun c -> c <> '(') |>> Normal

let parseStringPart = parseRepeat <|> parseNormalPart

let parseParts s =
    match run (many parseStringPart) s with
    | Success (stringPart, _, _) -> stringPart
    | Failure _ -> failwith "fail"

let rec length =
    function
    | Repeat (n, s) -> n * (s |> List.sumBy length)
    | Normal s -> int64 s.Length

let decompressedLength s =
    parseParts s
    |> List.sumBy length

File.ReadAllLines(inputPath)
|> Array.sumBy decompressedLength

/// PART 2

let input = File.ReadAllLines(inputPath).[0]

let rec expand =
    function
    | Repeat (n, s) ->
        Repeat (n, s |> List.map expand)
    | Normal s ->
        let newParts = parseParts s
        match newParts with
        | [ Normal t ] when t = s -> Normal s
        | _ -> Repeat (1L, newParts |> List.map expand) 
        
Repeat (1L, parseParts input)
|> expand
|> length