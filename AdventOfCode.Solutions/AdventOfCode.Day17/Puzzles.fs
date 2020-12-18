namespace AdventOfCode.Day17

open System

module Puzzles =
    let describe =
        "Conway Cubes"
        
    let rec cartesian lst =
        let inner n lst =
            match lst with
            | [] -> [[n]]
            |  _ -> List.map(fun l -> n::l) lst
        
        match lst with
        | []   -> []
        | h::t -> List.collect(fun n -> inner n (cartesian t)) h

    let simulatePoint (field: Array) (index: int[]) (mods: int list list) =
        let aneighbors = mods |> List.map(fun l -> l |> List.mapi(fun i j -> index.[i] + j)) |> List.ofSeq
        let vneighbors = aneighbors |> List.where(fun l -> l |> List.mapi(fun i v -> (i, v)) |> List.forall(fun (i, v) -> v >= 0 && v < field.GetLength(i))) |> List.ofSeq
        
        let alivecount = vneighbors |> List.where(fun l -> (field.GetValue((l |> Array.ofSeq)) :?> char) = '#') |> List.length
        let fieldvalue = field.GetValue(index) :?> char
        
        if fieldvalue = '#' && (aneighbors.Length <> vneighbors.Length) then 
            printfn "Alive on the edge - the calculation will be incorrect"
        
        if fieldvalue = '#' && (alivecount = 2 || alivecount = 3) then '#'
        else if (fieldvalue = '.' || fieldvalue = '\000') && alivecount = 3 then '#'
        else '.'

    let rec simulateField2 (field: Array) (step: int) (dims: int[]) (indexes: int list list) (mods: int list list) =
        printfn "Simulating rank %d step %d" field.Rank step

        if step = 0 then
            let mutable count = 0
            for index in indexes do
                let aindex = index |> Array.ofList
                let avalue = field.GetValue(aindex) :?> char
                count <- count + (if avalue = '#' then 1 else 0)
            count
        else
            let nfield = Array.CreateInstance(typedefof<char>, dims)
            for index in indexes do
                let aindex = index |> Array.ofList
                nfield.SetValue(simulatePoint field aindex mods, aindex)
            simulateField2 nfield (step - 1) dims indexes mods

    let rec simulateField (field: Array) (step: int) =
        let dims = { 0 .. (field.Rank - 1) } |> Seq.map(fun x -> field.GetLength(x)) |> Array.ofSeq
        let ranks = { 0 .. (field.Rank - 1) } |> Seq.map(fun m -> {0 .. (field.GetLength(m) - 1)} |> List.ofSeq) |> List.ofSeq

        let indexes = cartesian ranks
        let mods = cartesian (List.init field.Rank (fun _ -> [-1; 0; 1])) |> List.where(fun l -> (l |> List.where(fun v -> v = 0) |> List.length) <> l.Length) |> List.ofSeq

        simulateField2 field step dims indexes mods

    let loadArrayInto (lines: string[]) (target: Array) (dims: int[]) =
        let xsize = lines.[0].Length
        let ysize = lines.Length

        let indexes = Array.init dims.Length (fun i -> dims.[i] / 2)
        indexes.[0] <- indexes.[0] - (xsize / 2)
        indexes.[1] <- indexes.[1] - (ysize / 2)
        for x = 0 to xsize - 1 do
            for y = 0 to ysize - 1 do
                let index = indexes |> Array.copy
                index.[0] <- index.[0] + x
                index.[1] <- index.[1] + y
                target.SetValue(lines.[y].[x], index)
        
        
    let execute (filenames: string list) =
        let dims3D = [30; 30; 14] |> Array.ofSeq
        let dims4D = [30; 30; 14; 14;] |> Array.ofSeq
        
        let space3D = Array.CreateInstance(typedefof<char>, dims3D)
        let space4D = Array.CreateInstance(typedefof<char>, dims4D)
        
        let lines = System.IO.File.ReadAllLines(filenames.Head)
        loadArrayInto lines space3D dims3D
        loadArrayInto lines space4D dims4D
        
        let field3D = simulateField space3D 6
        let field4D = simulateField space4D 6
        
        printfn ""
        printfn "%d cubes remain alive in 3D space" field3D
        printfn "%d cubes remain alive in 4D space" field4D