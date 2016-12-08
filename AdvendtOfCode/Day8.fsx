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

let instructions =
    File.ReadAllLines(inputPath)
    |> Array.map parseInstruction

type ScreenElement = | ScreenElement of x : int * y : int * on : bool

let applyRect wide tall elems =
    let f (ScreenElement (x,y,_) as e) =
        if (x < wide && y < tall) then
            ScreenElement (x,y,true)
        else
            e
    
    elems
    |> Seq.map f

let applyRotateRow row pixels totalWidth elems =
    let f (ScreenElement (x,y,on) as e) =
        if (y = row) then
            ScreenElement ((x + pixels) % totalWidth, y, on)
        else
            e

    elems
    |> Seq.map f

let applyRotateColumn col pixels totalHeight elems =
    let f (ScreenElement (x,y,on) as e) =
        if (x = col) then
            ScreenElement (x, (y + pixels) % totalHeight, on)
        else
            e

    elems
    |> Seq.map f

let applyInstruction totalWidth totalHeight elems inst =
    match inst with
    | Rect (wide, tall) -> applyRect wide tall elems
    | RotateRow (row, pixels) -> applyRotateRow row pixels totalWidth elems
    | RotateColumn (column, pixels) -> applyRotateColumn column pixels totalHeight elems

let inputWidth = 50
let inputHeight = 6

let fInstruction elems inst = applyInstruction inputWidth inputHeight elems inst

let elements =
    seq {
        for x in 0 .. inputWidth - 1 do
            for y in 0 .. inputHeight - 1 do
                yield ScreenElement (x, y, false)
    }

instructions
|> Seq.fold fInstruction elements
|> Seq.filter (fun (ScreenElement (_,_,on)) -> on)
|> Seq.length