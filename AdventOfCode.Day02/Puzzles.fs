namespace AdventOfCode.Day02

open System.Text.RegularExpressions

module Puzzles =
    let describe = 
        "Password Philosophy"

    let rec checkPassword1 (min, max, chr, (lst: char list)) curr =
        if lst.IsEmpty then min <= curr && curr <= max
        else
            let head = lst.Head
            let next = if head = chr then curr + 1 else curr

            checkPassword1 (min, max, chr, lst.Tail) next

    let rec checkPassword2 (pos1, pos2, chr, (lst: char list)) =
        let idx1 = pos1 - 1
        let idx2 = pos2 - 1

        (lst.[idx1] = chr && lst.[idx2] <> chr) || (lst.[idx2] = chr && lst.[idx1] <> chr)
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)
        let regex = new Regex("(\d+)-(\d+) ([a-z]): ([a-z]+)")
        let policies = lines 
                           |> List.ofSeq 
                           |> List.map(fun x -> regex.Match(x)) 
                           |> List.map(fun x -> (Operators.int(x.Groups.[1].Value), Operators.int(x.Groups.[2].Value), Operators.char(x.Groups.[3].Value), [for c in x.Groups.[4].Value -> c]))

        let valid1 = policies |> List.filter(fun x -> checkPassword1 x 0) |> List.length
        let valid2 = policies |> List.filter(fun x -> checkPassword2 x) |> List.length

        printfn "Total passwords: %d" policies.Length
        printfn "Valid passwords in the first policy: %d" valid1
        printfn "Valid passwords in the second policy: %d" valid2
