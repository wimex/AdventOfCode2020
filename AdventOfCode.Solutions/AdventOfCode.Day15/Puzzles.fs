namespace AdventOfCode.Day15

module Puzzles =
    let describe =
        "Rambunctious Recitation"

    let rec say (position: int) (numbers: (int * int * bool)[]) (previous: int) =
        if position = numbers.Length then previous
        else
            let (pindex1, pindex2, _) = numbers.[previous]
            let next = pindex2 - pindex1

            let (_, nindex2, spoken) = numbers.[next]
            numbers.[next] <- if spoken then (nindex2, position, true) else (position, position, true)

            say (position + 1) numbers next


    let execute (filenames: string list) =
        let sequence = (System.IO.File.ReadAllLines(filenames.Head) |> Seq.head).Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> Seq.map(Operators.int) |> List.ofSeq
     
        //Using an array seems to be faster in this case, a Map can find an item in O(logn) but an array can find it in O(1)
        //There aren't that many elements so memory is not an issue (around 500MB)
        let array1 = Array.create 2020 (0, 0, false)
        let array2 = Array.create 30000000 (0, 0, false)
     
        let _ = sequence |> Seq.mapi(fun i x -> array1.[x] <- (i, i, true)) |> List.ofSeq
        let _ = sequence |> Seq.mapi(fun i x -> array2.[x] <- (i, i, true)) |> List.ofSeq

        let last1 = say sequence.Length array1 sequence.[sequence.Length - 1]
        printfn "Number before the elves get sick of playing: %d" last1

        let last2 = say sequence.Length array2 sequence.[sequence.Length - 1]
        printfn "Number before dinner is ready: %d" last2