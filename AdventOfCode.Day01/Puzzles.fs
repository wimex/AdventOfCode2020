namespace AdventOfCode.Day01

module Puzzles =
    let describe =
        "Report Repair"

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)
        let numbers = lines |> List.ofSeq |> List.map(Operators.int)

        let pairs = numbers |> List.collect(fun x -> numbers |> List.map(fun y -> (x, y, x + y)) |> List.filter(fun (_, _, s) -> s = 2020))
        let triplets = numbers |> List.collect(fun x -> numbers |> List.collect(fun y -> numbers |> List.map(fun z -> (x, y, z, x + y + z)) |> List.filter(fun (_, _, _, s) -> s = 2020)))

        let (p1, p2, _) = pairs.Head
        let (t1, t2, t3, _) = triplets.Head

        printfn "Puzzle 1: %d" (p1 * p2)
        printfn "Puzzle 2: %d" (t1 * t2 * t3)