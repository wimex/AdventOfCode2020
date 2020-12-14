namespace AdventOfCode.Day03

module Puzzles =
    let describe =
        "Toboggan Trajectory"

    let rec countTrees (field: char[,]) sx sy px py trees =
        let width = field.GetLength 0
        let height = field.GetLength 1

        if py >= height then
            trees
        else
            let ntrees = if field.[px,py] = '#' then trees + 1 else trees
            let npx = if (px + sx) >= width then (px + sx) % width else px + sx
            let npy = py + sy

            countTrees field sx sy npx npy ntrees

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)
        let field = Array2D.init lines.[0].Length lines.Length (fun x y -> lines.[y].[x])
        
        let rule1 = countTrees field 1 1 0 0 0 |> int64
        let rule2 = countTrees field 3 1 0 0 0 |> int64
        let rule3 = countTrees field 5 1 0 0 0 |> int64
        let rule4 = countTrees field 7 1 0 0 0 |> int64
        let rule5 = countTrees field 1 2 0 0 0 |> int64
        let total = rule1 * rule2 * rule3 * rule4 * rule5

        printfn "Rule 1: %d trees" rule1
        printfn "Rule 2: %d trees (<- question 1)" rule2
        printfn "Rule 3: %d trees" rule3
        printfn "Rule 4: %d trees" rule4
        printfn "Rule 5: %d trees" rule5
        printfn "Multiplied: %d" total