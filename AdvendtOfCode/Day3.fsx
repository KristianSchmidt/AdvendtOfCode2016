open System
open System.IO

let inputPath = Path.Combine(__SOURCE_DIRECTORY__,  @"day3.txt")

File.ReadAllLines(inputPath)
|> Array.map (fun s -> s.Split(' ') |> Array.filter ((<>)"") |> Array.map int |> Array.sort)
|> Array.filter (fun arr -> arr.[0] + arr.[1] > arr.[2])
|> Array.length

/// PART 2

File.ReadAllLines(inputPath)
|> Array.collect (fun s -> s.Split(' ') |> Array.filter ((<>)"") |> Array.map int)
|> Array.mapi (fun i d -> i % 3,d)
|> Array.groupBy fst
|> Array.map (snd >> Array.map snd)
|> Array.collect (Array.chunkBySize 3)
|> Array.map Array.sort
|> Array.filter (fun arr -> arr.[0] + arr.[1] > arr.[2])
|> Array.length