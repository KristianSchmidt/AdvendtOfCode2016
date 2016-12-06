open System
open System.IO
open System.Text
open System.Security.Cryptography
#time "on"

let md5 =
    let md5 = MD5.Create()
    fun (s : string) ->
        let bytes = Encoding.ASCII.GetBytes(s)
        let hash = md5.ComputeHash(bytes)
        let sb = StringBuilder()
        for i in 0 .. hash.Length - 1 do
            sb.Append(hash.[i].ToString("X2")) |> ignore
        sb.ToString()

let inputStr = "wtnhxymk"

let findNext offset =
    Seq.initInfinite (fun i -> i + offset + 1)
    |> Seq.find
        (fun i ->
            let hash = md5 (inputStr + i.ToString())
            hash.StartsWith("00000")
        )

let findPassword n =
    [| 1 .. n |]
    |> Array.fold
        (fun s _ ->
            let nxt = s |> List.head |> findNext
            nxt :: s) [0]
    |> List.rev
    |> List.tail
    |> List.map (fun i -> (md5 (inputStr + i.ToString())).Chars 5)
    |> (fun s -> String.Join("", s))

//findPassword 8


/// PART 2

let findNext2 offset positionsTaken =
    Seq.initInfinite (fun i -> i + offset + 1)
    |> Seq.find
        (fun i ->
            let hash = md5 (inputStr + i.ToString())
                
            if (hash.StartsWith("00000")) then
                match hash.Chars 5 with
                | '0' -> positionsTaken |> Set.contains '0' |> not
                | '1' -> positionsTaken |> Set.contains '1' |> not
                | '2' -> positionsTaken |> Set.contains '2' |> not
                | '3' -> positionsTaken |> Set.contains '3' |> not
                | '4' -> positionsTaken |> Set.contains '4' |> not
                | '5' -> positionsTaken |> Set.contains '5' |> not
                | '6' -> positionsTaken |> Set.contains '6' |> not
                | '7' -> positionsTaken |> Set.contains '7' |> not
                | _ -> false
            else
                false
        )

let findPassword2 n =
    [| 1 .. n |]
    |> Array.fold
        (fun (s,pos) _ ->
            let nxt = findNext2 (s |> List.head) pos
            let newPos = (md5 (inputStr + nxt.ToString())).Chars 5
            nxt :: s, pos.Add newPos) ([0], Set.empty)
    |> fst
    |> List.rev
    |> List.tail
    |> List.map (fun i ->
                    let str = md5 (inputStr + i.ToString())
                    str.Chars 5, str.Chars 6
                )
    |> List.sortBy fst
    |> List.map snd
    |> (fun s -> String.Join("", s))

findPassword2 8
