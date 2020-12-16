namespace AdventOfCode.Day16

open System
open System.Linq;

module Puzzles =
    let describe =
        "Ticket Translation"

    type Rule = string * int * int * int * int

    let rec getFieldOptions (ticket: string list list) position (fieldlist: Map<int, string list list>) =
        if ticket.IsEmpty then fieldlist
        else
            let head = ticket.Head
            let value = if fieldlist.ContainsKey(position) then head :: fieldlist.[position] else [head]
            let nfieldlist = fieldlist.Add(position, value)
            getFieldOptions ticket.Tail (position + 1) nfieldlist

    let rec getTicketOptions (validtickets: string list list list) (fieldlist: Map<int, string list list>) =
        if validtickets.IsEmpty then fieldlist
        else
            let ticket = validtickets.Head
            let nfieldlist = getFieldOptions ticket 0 fieldlist
            getTicketOptions validtickets.Tail nfieldlist

    let rec addFieldsToMap (list: (int * string) list) (fieldlist: Map<int, string>) =
        if list.IsEmpty then fieldlist
        else 
            let (key, value) = list.Head
            addFieldsToMap list.Tail (fieldlist.Add(key, value))

    let rec removeFieldsFromMap (list: (int * string) list) (fieldlist: Map<int, string list>) =
        if list.IsEmpty then fieldlist
        else
            let (key, _) = list.Head
            removeFieldsFromMap list.Tail (fieldlist.Remove(key))

    //For every valid ticket, collect the rules that appear with every field
    let getRowCandidates (validtickets: string list list list) =
        let options = getTicketOptions validtickets Map.empty
        let candidates = options |> Map.map(fun _ v -> v |> List.reduce(fun x y -> x.Intersect(y) |> List.ofSeq))
        candidates

    //From the candidate rows, select the one that has one possible field remaining
    let rec getRows (candidates: Map<int, string list>) (rows: Map<int, string>) =
        if candidates.IsEmpty then rows
        else 
            let rcs = candidates |> Map.filter(fun _ v -> v.Length = 1) |> Map.toList |> List.map(fun (k, v) -> (k, v.Head))
            if rcs.IsEmpty then raise(Exception("Unable to find the next candidate"))
            else
                let nrows = addFieldsToMap rcs rows
                let ncandidates1 = removeFieldsFromMap rcs candidates
                let ncandidates2 = ncandidates1 |> Map.map(fun _ v -> v |> List.where(fun x -> not (nrows |> Map.exists(fun _ m -> m = x))) |> List.ofSeq)
                getRows ncandidates2 nrows
                
    //For every field, enumerate every rule and return: the name, the value and the validity
    let validateField (rules: Rule list) (field: int) =
        rules |> List.map(fun (n, a, b, c, d) -> (n, field, (a <= field && field <= b) || (c <= field && field <= d)))

    let rec validateAllFields (rules: Rule list) (tickets: (int list) list) fieldlist =
        if tickets.IsEmpty then fieldlist
        else
            let ticket = tickets.Head
            let validation = ticket |> List.map(fun x -> validateField rules x)
            validateAllFields rules tickets.Tail (validation :: fieldlist)

    let rec getTickets (lines: string list) (tickets: (int list) list) =
        if lines.IsEmpty then tickets
        else
            let head = lines.Head
            let ticket = head.Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> Array.map(fun x -> int(x)) |> List.ofSeq
            getTickets lines.Tail (ticket :: tickets)

    let rec getRules (lines: string list) (rules: Rule list) =
        if lines.IsEmpty then rules
        else
            let head = lines.Head
            let regex = new System.Text.RegularExpressions.Regex("([A-z ]+): (\d+)-(\d+) or (\d+)-(\d+)")
            let matches = regex.Match head
            getRules lines.Tail ((matches.Groups.[1].Value, int(matches.Groups.[2].Value), int(matches.Groups.[3].Value), int(matches.Groups.[4].Value), int(matches.Groups.[5].Value)) :: rules)

    //These LINQ queries are getting out of hand! The elves are starting to panic...
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> List.ofSeq
        let block1 = lines |> List.findIndex(fun x -> x.Contains("your ticket"))
        let block2 = lines |> List.findIndex(fun x -> x.Contains("nearby tickets"))

        let ruleLines = lines |> List.take (block1 - 1) |> List.ofSeq
        let ticketLines = lines |> List.skip (block1 + 1) |> List.take (1) |> List.ofSeq
        let nearbyLines = lines |> List.skip (block2 + 1) |> List.ofSeq

        let rules = getRules ruleLines List.empty |> List.rev
        let ticket = getTickets ticketLines List.empty |> List.head |> Array.ofSeq
        let nearby = getTickets nearbyLines List.empty |> List.rev

        let validation = validateAllFields rules nearby List.empty

        //For every ticket, select those that has fields where none of the rules are valid
        //After that, select the field value from the first invalid rule and sum every invalid field in every ticket
        let invalidtickets = validation |> List.where(fun f -> f |> List.exists(fun v -> not (v |> List.exists(fun (_, _, r) -> r = true))))
        let invalidsum = invalidtickets  //For every ticket where none of the rules are valid 
                            |> List.map(fun f -> f |> List.where(fun v -> v |> List.forall(fun (_, _, r) -> r = false)) |> List.map(fun v -> v.Head))
                            |> List.map(fun f -> f |> List.sumBy(fun (_, v, _) -> v))
                            |> List.sum
        printfn "Completely invalid fields: %d" invalidsum
        
        //Select all valid tickets and select all the fields that are valid for that field
        let validtickets = validation |> List.where(fun f -> not (invalidtickets |> List.contains f))
        let validfields = validtickets |> List.map(fun f -> f |> List.map(fun v -> v |> List.where(fun (_, _, r) -> r = true) |> List.map(fun (n, _, _) -> n)))
        let candidates = getRowCandidates validfields
        let rows = getRows candidates Map.empty

        let departures = rows |> Map.filter(fun _ v -> v.StartsWith("departure"))
        let fieldsum = if not departures.IsEmpty then departures |> Map.map(fun i _ -> ticket.[i]) |> Map.toList |> List.map(fun (_,v) -> int64(v)) |> Seq.reduce((*)) else 0L

        printfn "Found row order: %s" (String.Join(", ", rows |> Map.toList |> List.map(fun (_, v) -> v)))
        printfn "Multiplication of the selected fields: %d" fieldsum