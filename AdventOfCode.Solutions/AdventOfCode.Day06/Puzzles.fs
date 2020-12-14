namespace AdventOfCode.Day06

module Puzzles =
    let describe =
        "Custom Customs"

    let intersect (string1: string) (string2: string) =
        System.String.Join("", System.Linq.Enumerable.Intersect(string1 |> List.ofSeq, string2 |> List.ofSeq))

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllText(filenames.Head)
        let groups = lines.Split("\n\n", System.StringSplitOptions.RemoveEmptyEntries)
        
        let unique1 = groups |> Seq.map(fun (l: string) -> l.Replace("\n", "")) |> List.ofSeq |> List.map(fun l -> l |> Seq.distinct |> Seq.length) |> Seq.reduce((+))
        let unique2 = groups |> Seq.map(fun (l: string) -> l.Split("\n", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.reduce(fun p e -> intersect p e) |> Seq.length) |> Seq.reduce((+))

        printfn "Sum of unique values for question 1: %d" unique1
        printfn "Sum of unique values for question 2: %d" unique2
