open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "day7.txt")

let splitStr (s : string) =
    let arr = s.Split([|']';'['|]) |> Array.mapi (fun i s -> i,s)
    let outsideBrackets =
        arr
        |> Array.filter (fun (i,_) -> i % 2 = 0)
        |> Array.map snd
    
    let insideBrackets =
        arr
        |> Array.filter (fun (i,_) -> i % 2 <> 0)
        |> Array.map snd
    
    outsideBrackets, insideBrackets

let containsAbba (s : string) =
    s.ToCharArray()
    |> Array.windowed 4
    |> Array.exists (fun arr -> arr.[0] = arr.[3] && arr.[1] = arr.[2] && arr.[0] <> arr.[1])

let supportsTLS s =
    let outside, inside = splitStr s
    outside |> Array.exists containsAbba && not (inside |> Array.exists containsAbba)

File.ReadAllLines inputPath
|> Array.filter supportsTLS
|> Array.length