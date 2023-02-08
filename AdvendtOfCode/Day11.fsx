#load "Helpers.fsx"

#time "on"

open System
open System.Text.RegularExpressions
open Helpers

let data = Helpers.Web.getInput 11

let data2 = """The first floor contains a hydrogen-compatible microchip, and a lithium-compatible microchip.
The second floor contains a hydrogen generator.
The third floor contains a lithium generator.
The fourth floor contains nothing relevant.""".Split("\n")

let getFloor s =
    Regex.Replace(s, "The \w+ floor contains a ", "")
    |> Helpers.split ", "
    |> Array.map (fun s -> s.Replace("and a ", ""))
    |> Array.map (fun s -> s.Replace("a ", ""))
    |> Array.map (
        function
        | Regex "(\w)\w+ generator" [c] -> (c.ToUpper()) + "G"
        | Regex "(\w)\w+\-compatible microchip" [c] -> (c.ToUpper()) + "M"
        )

let input =
    data
    |> Array.take 3
    |> Array.mapi (fun i s -> i+1, getFloor s |> Set.ofArray)
    |> Map.ofArray
    |> Map.add 4 Set.empty

let makeHash floor (map : Map<int,Set<string>>) =
    map
    |> Map.toSeq
    |> Seq.collect (fun (i,s) -> s |> Set.toSeq |> Seq.map (fun s -> (string i) + s))
    |> Seq.sort
    |> (fun s -> (string floor) + String.Join("",s))

let isValidState (map : Map<int,Set<string>>) =
    let isValidFloor (s : Set<string>) =
        let chips = s |> Set.filter (fun e -> e.EndsWith("M")) |> Set.map (fun s -> s.Substring(0,1))
        let rtgs = s |> Set.filter (fun e -> e.EndsWith("G"))
        let noRtgs = Set.isEmpty rtgs
        // For all chips it must hold that either there are no rtgs at all or the set should contain the chip's RTG
        chips |> Set.forall (fun c -> noRtgs || (rtgs |> Set.contains (c + "G")))

    map |> Map.forall (fun k v -> isValidFloor v)

let generateNextStates (floor : int,map : Map<int,Set<string>>) =
    // Get elements on current floor
    // Select all subsets of size 1 and 2
    // All those subsets can be moved either up or down (depending on which floor you are at)
    // Check all those subsets for validity and then output
    let elements = map[floor]
    let subsets =
        if (elements.Count = 1) then
            [elements]
        else
            let arr = elements |> Set.toArray
            seq {
                for i in 0 .. arr.Length-2 do
                    yield Set.ofArray [|arr[i]|]
                    for j in i+1 .. arr.Length-1 do
                        yield Set.ofArray [|arr[i];arr[j]|]
                yield Set.ofArray [|arr[arr.Length-1]|]
            }
            |> Seq.toList
    
    // If we're not on the minimum non-empty floor, we can move things down
    // otherwise not
    let yieldBelow =
        floor <> (map |> Map.filter (fun _ v -> not v.IsEmpty) |> Map.toSeq |> Seq.map fst |> Seq.min)
    subsets
    |> List.collect (fun subset ->
        let nextCurrFloor = Set.difference map[floor] subset
        let nextAbove = if (floor <> 4) then Some (Set.union map[floor+1] subset) else None
        let nextBelow = if (floor <> 1) then Some (Set.union map[floor-1] subset) else None
        seq {
            let baseMap = map |> Map.add floor nextCurrFloor
            if (nextAbove.IsSome) then
                yield floor+1, baseMap |> Map.add (floor+1) nextAbove.Value
            if (nextBelow.IsSome && yieldBelow) then
                yield floor-1, baseMap |> Map.add (floor-1) nextBelow.Value
        } |> Seq.toList
        )
    |> List.filter (snd >> isValidState)

let isFinished (map : Map<int,Set<string>>) =
    map[1].IsEmpty && map[2].IsEmpty && map[3].IsEmpty

let mutable minSteps = Int32.MaxValue
let mutable (seen : Map<string, int>) = Map.empty
let s = new System.Diagnostics.Stopwatch()
let rec solve (floor, map) hash step =
    seen <- seen.Add (hash, step)
    if (step >= minSteps) then
        ()
    else if (isFinished map) then
        if (step < minSteps) then
            printfn "New min: %i - %A" step s.Elapsed
            minSteps <- step
        else
            ()
    else
        generateNextStates (floor, map)
        |> List.map (fun (f,m) -> makeHash f m, (f,m))
        |> List.filter (fun (hash,(f,m)) ->
            let minMovesLeft = (3 * m[1].Count + 2 * m[2].Count + m[3].Count)
            let canMakeIt = step + minMovesLeft <= minSteps
            match Map.tryFind hash seen with
            | Some s -> step <= s && canMakeIt
            | None -> canMakeIt
            )
        |> List.sortByDescending (fun (_,(_,m)) -> 1000*m[4].Count + 100*m[3].Count + 10*m[2].Count + m[1].Count)
        |> List.iter (fun (h,c) -> solve c h (step+1))

s.Start()
solve (1, input) (makeHash 1 input) 0

// 833 too high

generateNextStates (1,input)
|> ((List.item 0) >> generateNextStates)
|> ((List.item 1) >> generateNextStates)
|> ((List.item 5) >> generateNextStates)
|> ((List.item 1) >> generateNextStates)
|> ((List.item 1) >> generateNextStates)
|> ((List.item 2) >> generateNextStates)
|> ((List.item 6) >> generateNextStates)
|> ((List.item 0) >> generateNextStates)
|> ((List.item 1) >> generateNextStates)
|> ((List.item 2) >> generateNextStates)


isValidState input

(*
Chips
RTGs

In other words, if a chip is ever left in the same area as another RTG,
and it's not connected to its own RTG, the chip will be fried.

there is an elevator that can move between the four floors.
Its capacity rating means it can carry at most yourself and two RTGs
or microchips in any combination.

As a security measure, the elevator will only function if it
contains at least one RTG or microchip.

The elevator always stops on each floor to recharge,
and this takes long enough that the items within it and the items on
that floor can irradiate each other.
(You can prevent this if a Microchip and its Generator end up
on the same floor in this way, as they can be connected
while the elevator is recharging.)

When you enter the containment area, you and the
elevator will start on the first floor.

*)

(*

New min: 57 - 00:00:00.0160716
New min: 55 - 00:00:03.8627450
New min: 53 - 00:04:35.1456560
New min: 51 - 00:04:37.8618098
New min: 49 - 00:07:26.5330880
New min: 47 - 00:53:36.7818189


*)