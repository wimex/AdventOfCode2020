namespace AdventOfCode.Day23

open System.Collections.Generic

module Puzzles =
    let describe = 
        "Crab Cups"

    let nextOrFirst (node: LinkedListNode<int>) =
        if node.Next <> null then node.Next
        else node.List.First

    let rec findIndexOf (min, max) (cache: Map<int, LinkedListNode<int>>) value =
        if value = (min - 1) then 
            findIndexOf (min, max) cache max
        else
            let (found, node) = cache.TryGetValue value
            if found && node.List <> null then node
            else findIndexOf (min,max) cache (value - 1)

    let rec shuffle (cache: Map<int, LinkedListNode<int>>) (minmax: int * int) (round: int) (item: LinkedListNode<int>) =
        if round = 0 then item.List
        else
            let a = nextOrFirst item
            let b = nextOrFirst a
            let c = nextOrFirst b
            
            item.List.Remove(a)
            item.List.Remove(b)
            item.List.Remove(c)

            let p = findIndexOf minmax cache (item.Value - 1)

            item.List.AddAfter(p, a)
            item.List.AddAfter(a, b)
            item.List.AddAfter(b, c)
            
            let n = nextOrFirst item
            let r = round - 1
            shuffle cache minmax r n

    let rec printResult (node: LinkedListNode<int>) =
        if node.Value = 1 then printfn ""
        else
            printf "%d" node.Value
            printResult (nextOrFirst node)
    
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllText(filenames.Head) |> Seq.map(fun f -> int(string(f))) |> List.ofSeq
        let circle1 = lines
        let circle2 = lines @ [(lines |> List.max) + 1 .. 1000000] |> List.ofSeq
        
        let list1 = new LinkedList<int>();
        let list2 = new LinkedList<int>();
        let cache1 = circle1 |> List.map(fun v -> (v, list1.AddLast(v))) |> Map.ofSeq
        let cache2 = circle2 |> List.map(fun v -> (v, list2.AddLast(v))) |> Map.ofSeq

        let min1 = circle1 |> List.min
        let max1 = circle1 |> List.max
        let min2 = circle2 |> List.min
        let max2 = circle2 |> List.max

        let _ = shuffle cache1 (min1, max1) 100 list1.First
        printf "Labels after 100 move: "
        printResult (nextOrFirst cache1.[1])

        let _ = shuffle cache2 (min2, max2) 10000000 list2.First
        printf "Labels after 10000000 move: "
        let first = nextOrFirst cache2.[1]
        let second = nextOrFirst first
        printfn "%d * %d = %d" first.Value second.Value (int64(first.Value) * int64(second.Value))