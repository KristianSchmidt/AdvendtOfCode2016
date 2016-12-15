open System
open System.IO
open System.Text.RegularExpressions

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "day10.txt")

type Entity = | Bot of int
              | Output of int

type Instructions = { Low : Entity; High : Entity }

type BotValues = | NoValues | OneValue of int | TwoValues of int*int

let hasTwoValues =
    function
    | TwoValues _ -> true
    | _ -> false

type State = { Outputs : Map<int,int>; BotValues : Map<int,BotValues>; Instructions : Map<int,Instructions> }

let [|notValueInst; valueInst |] =
    File.ReadAllLines(inputPath)
    |> Array.groupBy (fun s -> s.StartsWith("value"))
    |> Array.map snd

let parseEntity s n =
    match s with
    | "bot" -> Bot n
    | "output" -> Output n

let parseInstruction (s : string) =
    let regex = Regex("bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)")
    let m = regex.Match(s)
    match m.Success with
    | true ->
        let botNum = m.Groups.[1].Value |> int
        let lowEntity = m.Groups.[2].Value
        let lowNum = m.Groups.[3].Value |> int
        let highEntity = m.Groups.[4].Value
        let highNum = m.Groups.[5].Value |> int 
        botNum, { Low = parseEntity lowEntity highNum; High = parseEntity highEntity highNum }
    | false ->
        failwithf "Failed parsing instructions for; %s" s

let parseInitValue (s : string) =
    let regex = Regex("value (\d+) goes to bot (\d+)")
    let m = regex.Match(s)
    int m.Groups.[2].Value, OneValue (int m.Groups.[1].Value)

let inst =
    notValueInst
    |> Array.map parseInstruction
    |> Map.ofArray

let convArray arr =
    match arr with
    | [| OneValue d |] -> OneValue d
    | [| OneValue d; OneValue d' |] -> TwoValues(d, d')

let initValues =
    valueInst
    |> Array.map parseInitValue
    |> Array.groupBy fst
    |> Array.map (fun tup -> fst tup, snd tup |> Array.map snd |> convArray)
    |> Map.ofArray

let initState = { Outputs = Map.empty; BotValues = initValues; Instructions = inst }

let step (s : State) =
    // Skal bruge:
    // Bots som har 2 værdier
    // Deres instruktioner
    // Hvilke bots der så får en værdi tilført
    // Gamle værdier skal slettes, og de nye skal have deres værdi tilføjet

    // lav funktion der kun kører een bot frem
    s.BotValues |> Map.filter (fun _ v -> hasTwoValues v)

step initState