open System
open System.IO
open System.Text.RegularExpressions

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "day10.txt")

type Entity = | Bot of int
              | Output of int

type Instructions = { Low : Entity; High : Entity }

type BotValues = | NoValues | OneValue of int | TwoValues of int*int

let addValue i =
    function
    | NoValues -> OneValue i
    | OneValue i' -> TwoValues (i,i')
    | TwoValues _ -> failwith "cannot add to twovalues"

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
        botNum, { Low = parseEntity lowEntity lowNum; High = parseEntity highEntity highNum }
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
    let botsToGive = s.BotValues |> Map.filter (fun _ v -> hasTwoValues v) |> Map.toArray |> Array.map fst

    let processOneBot (s : State) botNum =
        let inst = s.Instructions |> Map.find botNum
        let valLow,valHigh = s.BotValues |> Map.find botNum |> (function | TwoValues (i,j) -> min i j, max i j | _ -> failwith ".")
        
        if (valLow = 17 && valHigh = 61) then
            printfn "bot %i" botNum
        let processInstruction recipient value state =
            match recipient with
            | Bot i ->
                let currVal = defaultArg (state.BotValues |> Map.tryFind i) NoValues
                let newVal = currVal |> addValue value
                { state with BotValues = state.BotValues |> Map.add i newVal }
            | Output i ->
                // PART 2
                if (i <= 2) then
                    printfn "Bucket %i: %i" i value
                { state with Outputs = state.Outputs |> Map.add i value}
        
        let addedState = s |> processInstruction inst.Low valLow |> processInstruction inst.High valHigh
        { addedState with BotValues = addedState.BotValues |> Map.remove botNum }
        
    botsToGive
    |> Array.fold processOneBot s

// Step through enough times to get the answer printed
[| 1 .. 100 |]
|> Array.fold (fun s _ -> step s) initState
