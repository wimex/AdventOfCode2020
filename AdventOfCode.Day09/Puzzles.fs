namespace AdventOfCode.Day09

module Puzzles =
    let describe =
        "Encoding Error"

    let rec findOutlier (numbers: int64[]) ws we =
        let head = numbers.[we + 1]
        let exists = numbers.[ws..we] 
                        |> Seq.collect(fun x -> numbers.[ws..we] |> Seq.map(fun y -> int64(x + y)) |> Seq.filter((=) head))
                        |> Seq.length = 0

        if exists then head
        else findOutlier numbers (ws + 1) (we + 1)
        
    let rec findSumsOfSize (numbers: int64[]) target position length =
        if (position + length) >= numbers.Length then (false, Array.empty)
        else
            let window = numbers.[position..(position + length)]
            let sum = window |> Seq.sum

            if sum = target then (true, window)
            else findSumsOfSize numbers target (position + 1) length

    let rec findSums (numbers: int64[]) target length =
        let (found, window) = findSumsOfSize numbers target 0 length
        if found then window
        else findSums numbers target (length + 1)
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Seq.map(int64) |> Array.ofSeq
        
        let outlier = findOutlier lines 0 24
        let weakness = findSums lines outlier 2
        let wmin = weakness |> Seq.min
        let wmax = weakness |> Seq.max

        printfn "The first invalid number is %d" outlier
        printfn "Encryption weakness: %d + %d = %d" wmin wmax (wmin + wmax)
