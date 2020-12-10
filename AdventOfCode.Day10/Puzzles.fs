namespace AdventOfCode.Day10

module Puzzles =
    let describe =
        "Adapter Array"

    let rec calculateDiffs (adapters: int list) countDiff1 countDiff3 =
        if adapters.Tail.IsEmpty then (countDiff1, countDiff3)
        else
            let head = adapters.Head
            let diff = adapters.Tail.Head - head

            let ndiff1 = if diff = 1 then countDiff1 + 1 else countDiff1
            let ndiff3 = if diff = 3 then countDiff3 + 1 else countDiff3

            calculateDiffs adapters.Tail ndiff1 ndiff3

    let rec createGraph (adapters: int list) (graph: Map<int, int64 * int seq>) =
        if adapters.IsEmpty then graph
        else
            let head = adapters.Head
            let valid = adapters.Tail |> Seq.where(fun x -> x <= (head + 3))
            let value = if adapters.Tail.IsEmpty then 1L else 0L
            createGraph adapters.Tail (graph.Add(head, (value, valid)))

    //The array is ordered BACKWARDS, so at every point the amount of arrangements (equalling
    //the amount of routes from that point until the last point) is the sum of the possible routes of
    //the PREVIOUS edges. Initially, the last point has a value of 1 (it can only participate as
    //the last element, so it's only part of one valid arrangement)
    let rec calculateRoutes (graph: Map<int, int64 * int seq>) (keys: int list) =
        let key = keys.Head
        let (_, edges) = graph.[key]
        let sums = edges |> Seq.map(fun x -> graph.[x] |> fst) |> Seq.sum
        let ngraph = graph |> Map.change key (fun _ -> Some (sums, edges))

        if keys.Tail.IsEmpty then sums
        else calculateRoutes ngraph keys.Tail

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)
        let numbers = lines |> Seq.map(Operators.int) |> Seq.sort |> List.ofSeq

        let adapters = (0 :: numbers) @[(numbers |> Seq.last) + 3]
        let graph = createGraph adapters Map.empty
        
        let (diff1, diff2) = calculateDiffs adapters 0 0
        printfn "Places with a difference of 1 and 3: %d * %d = %d" diff1 diff2 (diff1 * diff2)

        let keys = adapters |> List.rev |> List.tail
        let routes = calculateRoutes graph keys
        printfn "Valid arrangements: %d" routes