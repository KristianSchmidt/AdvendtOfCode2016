#load "Helpers.fsx"

open System
open System.Text.RegularExpressions
open Helpers

let data = Helpers.Web.getInput 11

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

let makeHash (map : Map<int,Set<string>>) =
    map
    |> Map.toSeq
    |> Seq.collect (fun (i,s) -> s |> Set.toSeq |> Seq.map (fun s -> (string i) + s))
    |> Seq.sort
    |> (fun s -> String.Join("",s))

makeHash input

let isValidState (map : Map<int,Set<string>>) =
    let isValidFloor (s : Set<string>) =
        let chips = s |> Set.filter (fun e -> e.EndsWith("M"))
        true

    map |> Map.forall (fun k v -> isValidFloor v)

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