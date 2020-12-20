namespace AdventOfCode.Day19

open System.Text.RegularExpressions

module Puzzles =
    let describe =
        "Monster Messages"

    let loop (left: string) (right: string) = 
        if not (right.Contains("LOOPDELOOP")) then "(" + left + "|" + right + ")"
        else
            let split = right.Split("LOOPDELOOP", System.StringSplitOptions.RemoveEmptyEntries)
            let n1 = if split.Length = 2 then "grpd" else "grps"
            let n2 = if split.Length = 2 then "-grpd" else "grps"
            
            //.NET can balance regex groups so the first part is captured the same amount of time as the second part.
            //This most probably fails on many other inputs but it works on my machine (tm)
            let ln = if split.Length > 0 then "(?<" + n1 + ">" + split.[0] + ")+" else ""
            let rn = if split.Length = 2 then "(?<" + n2 + ">" + split.[1] + ")+" else ""
            
            ln + rn

    let rec parse (id: int) (rules: (int * string) list) (result: string) =
        if rules.IsEmpty then result
        else
            let (_, rule) = rules |> List.where(fun (i, _) -> i = id) |> List.head
            if rule.Contains("\"") then rule.Trim().Trim('\"')
            else if rule.Contains("|") then
                let split = rule.Split("|", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun l -> l.Trim()) |> List.ofSeq
                let numbers = split |> Seq.map(fun s -> s.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun i-> int(i)) |> List.ofSeq) |> List.ofSeq
                let parsed = numbers |> Seq.map(fun p -> p |> List.map(fun i -> if i <> id then parse i rules result else "LOOPDELOOP")) |> List.ofSeq
                let left = parsed.[0] |> String.concat ""
                let right = parsed.[1] |> String.concat ""
                let result = loop left right
                result
            else 
                let numbers = rule.Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun i-> int(i)) |> List.ofSeq
                let parsed = numbers |> List.map(fun i -> parse i rules result) |> List.ofSeq
                parsed |> String.concat ""

    let read (filename: string) =
        let lines = System.IO.File.ReadAllLines(filename) |> Seq.map(fun x -> x.Split(":", System.StringSplitOptions.RemoveEmptyEntries)) |> List.ofSeq
        
        let rules = lines |> List.where(fun l -> l.Length = 2) |> List.map(fun l -> (int(l.[0]), l.[1])) |> List.ofSeq
        let messages = lines |> List.where(fun l -> l.Length = 1) |> List.ofSeq

        (rules, messages)
        
    let execute (filenames: string list) =
        let (rules1, messages1) = read filenames.[0]
        let (rules2, messages2) = read filenames.[1]

        let rule1 = parse 0 rules1 ""
        let regex1 = new Regex("^" + rule1 + "$")
        let count1 = messages1 |> List.map(fun i -> regex1.IsMatch(i.[0])) |> List.where(fun m -> m) |> List.length

        let rule2 = parse 0 rules2 ""
        let regex2 = new Regex("^" + rule2 + "$")
        let count2 = messages2 |> List.map(fun i -> regex2.IsMatch(i.[0])) |> List.where(fun m -> m ) |> List.length

        printfn "%d messages without loops" count1
        printfn "%d messages with loops" count2