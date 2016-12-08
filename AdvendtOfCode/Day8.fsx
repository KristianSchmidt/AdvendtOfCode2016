open System
open System.IO
open System.Text.RegularExpressions

let inputPath = Path.Combine(__SOURCE_DIRECTORY__, "day8.txt")

type Instruction =
    | Rect of wide : int * tall : int
    | RotateRow of row : int * pixels : int
    | RotateColumn of column : int * pixels : int

// grabs two ints from a regex and applies f to them if there's a match
let convertRegex (r : Regex) s f =
    let m = r.Match(s)
    if (m.Success) then
        let val1 = int m.Groups.[1].Value
        let val2 = int m.Groups.[2].Value
        f (val1, val2) |> Some
    else
        None

let parseInstruction (s : string) =
    let rect = Regex("rect (\d+)x(\d+)")
    let rotateRow = Regex("rotate row y=(\d+) by (\d+)")
    let rotateColumn = Regex("rotate column x=(\d+) by (\d+)")

    [ Rect, rect
      RotateRow, rotateRow
      RotateColumn, rotateColumn ]
    |> List.pick (fun (f, regex) -> convertRegex regex s f)

File.ReadAllLines(inputPath)
|> Array.map parseInstruction