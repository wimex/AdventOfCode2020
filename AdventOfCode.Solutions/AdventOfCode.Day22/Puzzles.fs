namespace AdventOfCode.Day22

module Puzzles =
    let describe =
        "Crab Combat"

    let getChecksumOfCards (cards: int64 list) =
        cards |> List.mapi(fun i v -> (int64(i), int64(v))) |> List.map(fun (i, v) -> (i + 1L) * v) |> List.sum

    let rec playRecursive (cards1: int64 list) (cards2: int64 list) (history: (int64 * int64) list) =
        if cards1.IsEmpty || cards2.IsEmpty then (cards1, cards2)
        else
            let head1 = cards1.Head
            let head2 = cards2.Head
            let len1 = int64(cards1.Tail.Length)
            let len2 = int64(cards2.Tail.Length)
            let chk = ((getChecksumOfCards cards1), (getChecksumOfCards cards2))

            if (history |> List.contains chk) then
                (cards1, List.empty)
            else if head1 <= len1 && head2 <= len2 then
                let rcards1 = cards1.Tail |> List.take (int(head1)) |> List.ofSeq
                let rcards2 = cards2.Tail |> List.take (int(head2)) |> List.ofSeq
                let (result1, result2) = playRecursive rcards1 rcards2 List.empty
                let ncards1 = if result2.IsEmpty then (cards1.Tail @ [head1; head2]) else cards1.Tail
                let ncards2 = if result1.IsEmpty then (cards2.Tail @ [head2; head1]) else cards2.Tail
                playRecursive ncards1 ncards2 (chk :: history)
            else
                let ncards1 = if head1 > head2 then (cards1.Tail @ [head1; head2]) else cards1.Tail
                let ncards2 = if head1 <= head2 then (cards2.Tail @ [head2; head1]) else cards2.Tail
                playRecursive ncards1 ncards2 (chk :: history)

    let rec playNormal (cards1: int64 list) (cards2: int64 list) =
        if cards1.IsEmpty || cards2.IsEmpty then (cards1, cards2)
        else
            let ncards1 = if cards1.Head > cards2.Head then (cards1.Tail @ [cards1.Head; cards2.Head]) else cards1.Tail
            let ncards2 = if cards1.Head <= cards2.Head then (cards2.Tail @ [cards2.Head; cards1.Head]) else cards2.Tail
            playNormal ncards1 ncards2 //Ironic!


    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)
        let players = lines |> Array.indexed |> Array.filter(fun (_, x) -> x.Contains("Player")) |> Array.map fst |> List.ofSeq
        let cards1 = lines |> Array.skip (players.[0] + 1) |> Array.splitAt (players.[1] - 2) |> fst |> Seq.map(fun i -> int64(i)) |> List.ofSeq
        let cards2 = lines |> Array.skip (players.[1] + 1) |> Seq.map(fun i -> int64(i)) |> List.ofSeq

        let (result11, result12) = playNormal cards1 cards2
        let score1 = (if result12.IsEmpty then result11 else result12) |> List.rev |> getChecksumOfCards

        let (result21, result22) = playRecursive cards1 cards2 List.empty
        let score2 = (if result22.IsEmpty then result21 else result22) |> List.rev |> getChecksumOfCards

        printfn "The winning of the first round is player %d with a score of %d" (if result12.IsEmpty then 1 else 2) score1
        printfn "The winning of the second round is player %d with a score of %d" (if result22.IsEmpty then 1 else 2) score2
        printfn "Bye"