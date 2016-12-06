open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "day6.txt")

let input = File.ReadAllLines(inputPath)

let commonInCol i =
    input
    |> Array.map (fun arr -> arr.[i])
    |> Array.countBy id
    |> Array.sortByDescending snd
    |> Array.head
    |> fst
    
[ 0 .. input.[0].Length - 1]
|> List.map commonInCol
|> (fun seq -> String.Join("", seq))

/// PART 2

let commonInCol2 i =
    input
    |> Array.map (fun arr -> arr.[i])
    |> Array.countBy id
    |> Array.sortBy snd
    |> Array.head
    |> fst
    
[ 0 .. input.[0].Length - 1]
|> List.map commonInCol2
|> (fun seq -> String.Join("", seq))
