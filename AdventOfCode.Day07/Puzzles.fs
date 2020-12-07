namespace AdventOfCode.Day07

open System.Text.RegularExpressions
open System

module Puzzles =
    let describe =
        "Handy Haversacks"

    let rec countTree1 (list: (string * ((string * int) list)) list) (candidates: string list) (visited: string list) (results: string list) =
        if candidates.IsEmpty then results    
        else
            let candidate = candidates.Head
            let containers = list |> List.filter(fun (_, contents) -> contents |> List.exists(fun (k, _) -> k = candidate)) |> List.map(fun (k, _) -> k) |> List.ofSeq
            
            let nvisited = candidate :: visited
            let nresults = results |> List.append containers |> List.distinct |> List.ofSeq
            let ncandidates = candidates.Tail |> List.append containers |> List.distinct |> List.filter(fun s -> not (nvisited |> List.contains s)) |> List.ofSeq

            countTree1 list ncandidates nvisited nresults

    let rec countTree2 (list: (string * ((string * int) list)) list) candidate sum =
        let (_, children) = list |> List.filter(fun (k, _) -> k = candidate) |> List.head
        if children.IsEmpty then sum
        else
            sum + (children |> List.map(fun (k, c) -> c * countTree2 list k 1) |> List.reduce((+)))
        
    
    let execute (filenames: string list) =
        let needle = "shiny gold"
        let regex = new Regex("(\d) ([a-z]+ [a-z]+) bag");

        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Seq.map(fun f -> f.Split("bags contain", StringSplitOptions.RemoveEmptyEntries) |> Seq.map(fun f -> f.Trim()))
        let parsed = lines |> Seq.map(fun l -> (Seq.head l, Seq.last l)) |> Seq.map(fun (n, d) -> (n, regex.Matches(d) |> Seq.map(fun t -> (t.Groups.[2].Value, t.Groups.[1].Value |> int)) |> List.ofSeq)) |> List.ofSeq

        let baglist1 = countTree1 parsed (seq {needle} |> List.ofSeq) List.empty List.empty
        let bagcount2 = countTree2 parsed needle 0

        printfn "%d bags can contain a(n) %s bag" baglist1.Length needle
        printfn "A(n) %s bag contains %d other bags" needle bagcount2
