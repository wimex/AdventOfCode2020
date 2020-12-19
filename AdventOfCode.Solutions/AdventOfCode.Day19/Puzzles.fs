namespace AdventOfCode.Day19

open System.Text.RegularExpressions

module Puzzles =
    let describe =
        ""
    
    type Rule =
        {
            value: string option
            left: Rule option
            right: Rule option
        }

    //let 

    //let rec parse (id: int) (rules: (int * string) list) (parent: Rule) =
    //    if rules.IsEmpty then parent
    //    else
    //        let (_, rule) = rules |> List.where(fun (i, _) -> i = id) |> List.head
    //        if rule.Contains("\"") then
    //            {Rule.value = Some(rule.Replace("\"", "").Trim()); Rule.left = None; Rule.right = None}
    //        else if rule.Contains("|") then
    //            let rrr = rule.Split("|", System.StringSplitOptions.RemoveEmptyEntries) 
    //                                    |> Array.map(fun r -> r.Split(" ", System.StringSplitOptions.RemoveEmptyEntries))
    //            parent
    //        else
    //            parent

    let rec parse (id: int) (rules: (int * string) list) (result: string) =
        if rules.IsEmpty then result
        else
            let (_, rule) = rules |> List.where(fun (i, _) -> i = id) |> List.head
            if rule.Contains("\"") then rule.Trim().Trim('\"')
            else if rule.Contains("|") then
                let split = rule.Split("|", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun l -> l.Trim()) |> List.ofSeq
                let numbers = split |> Seq.map(fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun i-> int(i)) |> List.ofSeq) |> List.ofSeq
                let parsed = numbers |> Seq.map(fun p -> p |> List.map(fun i -> parse i rules result)) |> List.ofSeq
                let left = parsed.[0] |> String.concat ""
                let right = parsed.[1] |> String.concat ""
                "(" + left + "|" + right + ")"
            else 
                let numbers = rule.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun i-> int(i)) |> List.ofSeq
                let parsed = numbers |> List.map(fun i -> parse i rules result) |> List.ofSeq
                parsed |> String.concat ""
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Seq.map(fun x -> x.Split(":", System.StringSplitOptions.RemoveEmptyEntries)) |> List.ofSeq
        let rules = lines |> List.where(fun l -> l.Length = 2) |> List.map(fun l -> (int(l.[0]), l.[1])) |> List.sortBy(fun (r, _) -> r) |> List.ofSeq
        let messages = lines |> List.where(fun l -> l.Length = 1) |> List.ofSeq

        let rule = parse 0 rules ""
        let regex = new Regex("^" + rule + "$")
        let matches = messages |> List.map(fun i -> regex.IsMatch(i.[0])) |> List.where(fun m -> m) |> List.length

        printfn "%d messages match rule 0" matches

        printfn "Bye"