open System
open System.IO

let inputPath = @"C:\Users\Kristian\Documents\Visual Studio 2015\Projects\AdvendtOfCode\AdvendtOfCode\day2.txt"

let nextMove pos move =
    match pos, move with
    | 1, 'R' -> 2
    | 1, 'D' -> 4
    | 2, 'R' -> 3
    | 2, 'D' -> 5
    | 2, 'L' -> 1
    | 3, 'D' -> 6
    | 3, 'L' -> 2
    | 4, 'U' -> 1
    | 4, 'R' -> 5
    | 4, 'D' -> 7
    | 5, 'U' -> 2
    | 5, 'R' -> 6
    | 5, 'D' -> 8
    | 5, 'L' -> 4
    | 6, 'U' -> 3
    | 6, 'D' -> 9
    | 6, 'L' -> 5
    | 7, 'U' -> 4
    | 7, 'R' -> 8
    | 8, 'U' -> 5
    | 8, 'R' -> 9
    | 8, 'L' -> 7
    | 9, 'U' -> 6
    | 9, 'L' -> 8
    | _ -> pos

let nextNumber pos (moves : string) =
    moves.ToCharArray()
    |> Array.fold nextMove pos

let code (moves : string array) =
    let folder code move =
        let pos = defaultArg (code |> List.tryHead) 5
        let next = nextNumber pos move
        next :: code
    
    moves
    |> Array.fold folder []
    |> List.rev

code <| File.ReadAllLines(inputPath)

/// PART 2

let nextMove2 pos move =
    match pos with
    | '1' ->
        match move with
        | 'D' -> '3'
        | _ -> pos
    | '2' ->
        match move with
        | 'D' -> '6'
        | 'R' -> '3'
        | _ -> pos
    | '3' ->
        match move with
        | 'U' -> '1'
        | 'D' -> '7'
        | 'L' -> '2'
        | 'R' -> '4'
        | _ -> pos
    | '4' ->
        match move with
        | 'D' -> '8'
        | 'L' -> '3'
        | _ -> pos
    | '5' ->
        match move with
        | 'R' -> '6'
        | _ -> pos
    | '6' ->
        match move with
        | 'U' -> '2'
        | 'D' -> 'A'
        | 'L' -> '5'
        | 'R' -> '7'
        | _ -> pos
    | '7' ->
        match move with
        | 'U' -> '3'
        | 'D' -> 'B'
        | 'L' -> '6'
        | 'R' -> '8'
        | _ -> pos
    | '8' ->
        match move with
        | 'U' -> '4'
        | 'D' -> 'C'
        | 'L' -> '7'
        | 'R' -> '9'
        | _ -> pos
    | '9' ->
        match move with
        | 'L' -> '8'
        | _ -> pos
    | 'A' ->
        match move with
        | 'U' -> '6'
        | 'R' -> 'B'
        | _ -> pos
    | 'B' ->
        match move with
        | 'U' -> '7'
        | 'D' -> 'D'
        | 'L' -> 'A'
        | 'R' -> 'C'
        | _ -> pos
    | 'C' ->
        match move with
        | 'U' -> '8'
        | 'L' -> 'B'
        | _ -> pos
    | 'D' ->
        match move with
        | 'U' -> 'B'
        | _ -> pos
    | _ -> pos

let nextNumber2 pos (moves : string) =
    moves.ToCharArray()
    |> Array.fold nextMove2 pos

let code2 (moves : string array) =
    let folder code move =
        let pos = defaultArg (code |> List.tryHead) '5'
        let next = nextNumber2 pos move
        next :: code
    
    moves
    |> Array.fold folder []
    |> List.rev

code2 <| File.ReadAllLines(inputPath)