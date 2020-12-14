namespace AdventOfCode.Day12

module Puzzles =
    let describe =
        "Rain Risk"
    
    type Waypoint =
        {
            Col: int
            Row: int
        }

    type Ferry =
        {
            Col: int
            Row: int
            Dir: string
        }

    let compass =
        ["N"; "E"; "S"; "W"]

    let rotate1 (ferry: Ferry) rotation length =
        let start = compass |> List.findIndex(fun c -> c = ferry.Dir)
        let clicks = length / 90
        let modifier = if rotation = "L" then -1 else 1
        let item = (start + clicks * modifier) % compass.Length 
        let result = if item < 0 then compass.Length + item else item

        {ferry with Dir = compass.[result]}

    let rec move1 ((instruction, length): string * int) (ferry: Ferry) =
        match instruction with
        | "N"       -> {ferry with Col = ferry.Col - length}
        | "S"       -> {ferry with Col = ferry.Col + length}
        | "E"       -> {ferry with Row = ferry.Row + length}
        | "W"       -> {ferry with Row = ferry.Row - length}
        | "L" | "R" -> rotate1 ferry instruction length
        | "F"       -> move1 (ferry.Dir, length) ferry
        |   _       -> raise(System.Exception("Invalid direction"))

    let rec navigate1 (directions: (string * int) list) (ferry: Ferry) =
        if directions.IsEmpty then ferry
        else
            let nferry = move1 directions.Head ferry
            navigate1 directions.Tail nferry

    let rec rotate2 (star: Waypoint) rotation length =
        if length = 0 then star
        else
            match rotation with
            | "L" -> rotate2 {star with Row = star.Col; Col = -1 * star.Row} rotation (length - 90)
            | "R" -> rotate2 {star with Row = -1 * star.Col; Col = star.Row} rotation (length - 90)
            |   _ -> raise(System.Exception("Invalid rotation"))
            
    let move2 ((instruction, length): string * int) (ferry: Ferry) (star: Waypoint) =
        match instruction with
        | "N"       -> (ferry, {star with Col = star.Col - length})
        | "S"       -> (ferry, {star with Col = star.Col + length})
        | "E"       -> (ferry, {star with Row = star.Row + length})
        | "W"       -> (ferry, {star with Row = star.Row - length})
        | "L" | "R" -> (ferry, rotate2 star instruction length)
        | "F"       -> ({ferry with Row = ferry.Row + (length * star.Row); Col = ferry.Col + (length * star.Col)}, star)
        |  _        -> raise(System.Exception("Invalid direction"))
            
    let rec navigate2 (directions: (string * int) list) (ferry: Ferry) (star: Waypoint) =
        if directions.IsEmpty then ferry
        else
            let (nferry, nstar) = move2 directions.Head ferry star
            navigate2 directions.Tail nferry nstar
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Seq.map(fun x -> (x.Substring(0, 1), int(x.Substring(1, x.Length - 1)))) |> List.ofSeq
        let ferry = {Ferry.Row = 0; Ferry.Col = 0; Ferry.Dir = "E"}
        let star = {Waypoint.Row = 10; Waypoint.Col = -1}

        let result1 = navigate1 lines ferry
        let distance1 = System.Math.Abs(result1.Row) + System.Math.Abs(result1.Col)

        let result2 = navigate2 lines ferry star
        let distance2 = System.Math.Abs(result2.Row) + System.Math.Abs(result2.Col)

        printfn "1st manhattan distance: %d" distance1
        printfn "2nd manhattan distance: %d" distance2