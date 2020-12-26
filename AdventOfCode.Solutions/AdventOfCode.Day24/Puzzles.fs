namespace AdventOfCode.Day24

module Puzzles =
    let describe =
        "Lobby Layout"

    type Position = int * int * int
    
    let mods = [
        (-1, +1,  0);
        ( 0, +1, -1);
        (+1,  0, -1);
        (+1, -1,  0);
        ( 0, -1, +1);
        (-1,  0, +1)
    ]

    let directionToCoords (direction: string) (position: Position)  =
        let (x, y, z) = position
        match direction with
        | "se" -> (2, (x, y + 1, z - 1))
        | "sw" -> (2, (x - 1, y + 1, z))
        | "nw" -> (2, (x, y - 1, z + 1))
        | "ne" -> (2, (x + 1, y - 1, z))
        |   _  ->
            match direction.[0 .. 0] with
            |  "e" -> (1, (x + 1, y, z - 1))
            |  "w" -> (1, (x - 1, y, z + 1))
            |   _  -> raise(System.Exception("Invalid direction: " + direction))

    let rec paintLines (steps: string) (position: Position) (tiles: Position list) (values: int list) =
        if steps.Length = 0 then
            let index = tiles |> List.tryFindIndex (fun t -> t = position)
            let value = if index.IsSome then System.Convert.ToInt32(not (System.Convert.ToBoolean(values.[index.Value]))) else 0

            let ntiles = if index.IsSome then position :: (tiles |> List.except [position]) else position :: tiles
            let nvalues = if index.IsSome then [value] @ values.[.. (index.Value - 1)] @ values.[(index.Value + 1) ..] else value :: values
            
            (ntiles, nvalues)
        else
            let split = (min steps.Length 2) - 1
            let step = steps.[0 .. split]

            let (l, (x, y, z)) = directionToCoords step position
            let nsteps = steps.[l .. ]
            let nposition = (x, y, z)

            paintLines nsteps nposition tiles values

    let rec paintTiles (inputs: string list) (tiles: Position list) (values: int list) =
        if inputs.IsEmpty then (tiles, values)
        else
            let input = inputs.Head
            let (ntiles, nvalues) = paintLines input (0, 0, 0) tiles values
            paintTiles inputs.Tail ntiles nvalues

    let rec playStep (position: int) (tiles: Position list) (values: int list) (ntiles: Position list) (nvalues: int list) =
        if position = tiles.Length then (ntiles, nvalues)
        else
            let (x, y, z) = tiles.[position]
            let coords = mods |> List.map(fun (mx, my, mz) -> (x + mx, y + my, z + mz)) |> List.ofSeq
            let prevs = coords |> List.map(fun c -> tiles |> List.tryFindIndex(fun x -> x = c)) |> List.ofSeq
            
            let value = values.[position]
            let bcount = prevs |> List.filter(fun l -> l.IsSome && values.[l.Value] = 0) |> List.length
            
            let left = position - 1
            let right = position + 1

            if value = 0 && (bcount = 0 || bcount > 2) then 
                playStep (position + 1) tiles values ntiles (nvalues.[.. left] @ [1] @ nvalues.[right ..])
            else if value = 1 && bcount = 2 then 
                playStep (position + 1) tiles values ntiles (nvalues.[.. left] @ [0] @ nvalues.[right ..])
            else
                playStep (position + 1) tiles values ntiles nvalues

    let rec expandTiles (position: int) (tiles: Position list) (values: int list) =
        if position = tiles.Length then (tiles, values)
        else
            let (x, y, z) = tiles.[position]
            let value = values.[position]

            if value = 1 then
                expandTiles (position + 1) tiles values
            else
                let coords = mods |> List.map(fun (mx, my, mz) -> (x + mx, y + my, z + mz)) |> List.ofSeq
                let expands = coords |> List.where(fun c -> not (tiles |> List.contains c)) |> List.ofSeq
                let invals = List.init expands.Length (fun _ -> 1)
                
                let ntiles = expands @ tiles
                let nvalues = invals  @ values

                expandTiles (position + 1) ntiles nvalues
                
    let rec playTiles (limit: int) (tiles: Position list) (values: int list) =
        printf "Round %d (%d tiles)     \r" limit tiles.Length

        if limit = 0 then (tiles, values)
        else
            let (ntiles1, nvalues1) = expandTiles 0 tiles values
            let (ntiles2, nvalues2) = playStep 0 ntiles1 nvalues1 ntiles1 nvalues1
            
            //The list could be compacted to speed things up by a lot but the speed is acceptable for now
            playTiles (limit - 1) ntiles2 nvalues2
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> List.ofSeq
        let limit = 100

        let (tiles1, values1) = paintTiles lines List.empty List.empty
        let (tiles2, values2) = playTiles limit tiles1 values1

        let blacks1 = values1 |> List.filter(fun v -> v = 0) |> List.length
        let blacks2 = values2 |> List.filter(fun v -> v = 0) |> List.length
        
        printfn "%d tiles are black at the beginning" blacks1
        printfn "%d tiles are black after %d turns" blacks2 limit
