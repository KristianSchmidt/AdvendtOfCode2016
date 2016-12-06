open System
open System.IO

let inputPath = @"C:\Users\Kristian\Documents\Visual Studio 2015\Projects\AdvendtOfCode\AdvendtOfCode\day4.txt"

let correctChecksum (letters : char array) =
    letters
    |> Array.filter ((<>)'-')
    |> Array.countBy id
    |> Array.groupBy snd
    |> Array.sortByDescending fst
    |> Array.collect (snd >> Array.sortBy fst)
    |> Array.map fst
    |> Array.take 5
    |> (fun arr -> String.Join("", arr))

let splitIdCh (idCh : string) =
    let arr = idCh.Split([|']';'['|])
    int arr.[0], arr.[1]

let validRooms =
    File.ReadAllLines(inputPath)
    |> Array.map (fun s -> let arr = s.Split('-')
                           arr.[0 .. arr.Length-2], arr.[arr.Length-1])
    |> Array.map (fun (letters, idCh) -> letters |> Array.collect (fun s -> s.ToCharArray()), splitIdCh idCh)
    |> Array.filter (fun (letters, (ID, ch)) -> (correctChecksum letters) = ch)

validRooms
|> Array.sumBy (snd >> fst)

/// PART 2

let rotateLetter (sector : int) (c : char)  =
    let numValue = int c - 97
    let newValue = (numValue + sector) % (122 - 97 + 1)
    char (newValue + 97)

let rotateAll sector chars =
    chars
    |> Array.map (rotateLetter sector)
    |> String

validRooms
|> Array.map (fun (letters, (ID, _)) -> rotateAll ID letters, ID)
|> Array.filter (fun (s,ID) -> s.Contains("north"))
|> Array.head
|> snd