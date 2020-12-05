namespace AdventOfCode.Day05

module Puzzles =
    let describe =
        "Binary Boarding"

    let partionNumber chr (low, high) =
        let diff = (high - 1) - low
        let center = diff / 2
        let pivot = low + center

        match chr with
        | ('B' | 'R') -> (pivot + 1, high)
        | ('F' | 'L') -> (low, pivot)
        |           _ -> raise(System.Exception("Unknown character in input"))

    let rec partitionSeat (cmds: char list) (low, high)=
        if cmds.IsEmpty then (low, high)
        else
            let c = cmds.Head
            let (nlow, nhigh) = partionNumber c (low, high)
            partitionSeat cmds.Tail (nlow, nhigh)

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) 
                    |> List.ofSeq 
                    |> List.map(fun (l: string) -> (l.Substring(0, 7), l.Substring(7, 3)))
                    |> List.map(fun (r, c) -> (partitionSeat (r |> List.ofSeq) (0, 127), partitionSeat (c |> List.ofSeq) (0, 7)))
                    |> List.map(fun ((r, _), (c, _)) -> (r, c, (r * 8 + c)))

        let ordered = lines |> List.sortByDescending(fun (_, _, i) -> i)
        let (hr, hc, hi) = ordered |> List.head    
        let (lr, lc, li) = ordered |> List.last
        let missing = {li..hi} |> Seq.filter(fun i -> not (lines |> List.exists(fun (_, _, j) -> i = j))) |> Seq.head
        
        printfn "Lowest seat id: %d %d -> %d" lr lc li
        printfn "Highest seat id: %d %d -> %d" hr hc hi
        printfn "Please take your seat: %d" missing

