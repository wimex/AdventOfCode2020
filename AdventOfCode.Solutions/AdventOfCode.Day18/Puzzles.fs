namespace AdventOfCode.Day18

module Puzzles =
    let describe =
        "Operation Order"

    let evalLeftParOrNumber s = s = "(" || (System.Int32.TryParse s |> fst)
    let evalLeftParOrPrecOrNumber s = s = "(" || s = "*" || (System.Int32.TryParse s |> fst)

    let rec popWhile (stack: string list) (result: string list) (evaluator: string -> bool) =
        if stack.IsEmpty || (evaluator(stack.Head)) then (stack, result)
        else popWhile stack.Tail (stack.Head :: result) evaluator

    let rec parse version (operations: string list) (stack: string list) (result: string list) =
        if operations.IsEmpty then
            let (_, nresult) = popWhile stack result (fun _ -> false)
            nresult |> List.rev
        else
            let chr = operations.Head
            match chr with
            | " " ->
                parse version operations.Tail stack result
            | "*" -> 
                let (nstack, nresult) = popWhile stack result (fun s -> evalLeftParOrNumber s)
                parse version operations.Tail (chr::nstack) nresult
            | "+" -> 
                let (nstack, nresult) = popWhile stack result (fun s -> if version = 1 then evalLeftParOrNumber s else evalLeftParOrPrecOrNumber s)
                parse version operations.Tail (chr::nstack) nresult
            | "(" ->
                parse version operations.Tail (chr::stack) result
            | ")" ->
                let (nstack, nresult) = popWhile stack result (fun s -> evalLeftParOrNumber s)
                parse version operations.Tail nstack.Tail nresult
            |  _  -> 
                parse version operations.Tail stack (chr :: result)

    let rec evaluate (operations: string list) (stack: string list) =
        if operations.IsEmpty then stack.Head
        else
            let chr = operations.Head
            match chr with
            | "+" | "*" ->
                let left = stack.Head
                let right = stack.Tail.Head
                let next = stack.Tail.Tail
                let op = if chr = "*" then (*) else (+)
                let result = op (int64(left)) (int64(right))
                evaluate operations.Tail (string(result) :: next)
            |  _  ->
                evaluate operations.Tail (chr :: stack)

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Array.map(fun x -> x |> Seq.map (fun x-> string(x)) |> List.ofSeq)

        let parsed1 = lines |> Seq.map(fun x -> parse 1 x List.empty List.empty) |> List.ofSeq
        let parsed2 = lines |> Seq.map(fun x -> parse 2 x List.empty List.empty) |> List.ofSeq

        let values1 = parsed1 |> List.map(fun x -> evaluate x List.empty) |> List.ofSeq
        let values2 = parsed2 |> List.map(fun x -> evaluate x List.empty) |> List.ofSeq

        let sum1 = values1 |> List.map(fun x -> int64(x)) |> List.sum
        let sum2 = values2 |> List.map(fun x -> int64(x)) |> List.sum
        
        printfn "Sum of all operations without precedence: %d" sum1
        printfn "Sum of all operations with precedence: %d" sum2