namespace AdventOfCode.Day08

module Puzzles =
    type RAM =
        {
            Instructions: (string * int * bool) list
            Pointer: int
            Accumulator: int64
            Terminated: bool
        }

    let describe =
        "Handheld Halting"

    let rec runCpu (memory: RAM) =
        let terminated = memory.Pointer >= memory.Instructions.Length
        let (_, _, visited) = if not terminated then memory.Instructions.[memory.Pointer] else ("", 0, false)

        if terminated || visited then {RAM.Instructions = memory.Instructions; RAM.Pointer = memory.Pointer; RAM.Accumulator = memory.Accumulator; RAM.Terminated = terminated}
        else
            let (instruction, parameter, _) = memory.Instructions.[memory.Pointer]
            let ninstructions = memory.Instructions |> List.mapi(fun idx (ins, par, vis) -> if idx = memory.Pointer then (ins, par, true) else (ins, par, vis))

            match instruction with
            | "acc" ->                        
                        let nmemory = {memory with RAM.Instructions = ninstructions; RAM.Pointer = memory.Pointer + 1; RAM.Accumulator = memory.Accumulator + int64(parameter)}
                        runCpu nmemory
            | "jmp" ->
                        let nmemory = {memory with RAM.Instructions = ninstructions; RAM.Pointer = memory.Pointer + parameter}
                        runCpu nmemory
            |     _ ->
                        let nmemory = {memory with RAM.Instructions = ninstructions; RAM.Pointer = memory.Pointer + 1}
                        runCpu nmemory

    let rec replaceRamValues (memory: RAM) position =
        let nindex = memory.Instructions |> List.indexed |> List.findIndex(fun (idx, (i, _, _)) -> idx > position && (i = "jmp" || i = "nop"))
        let ninstructions = memory.Instructions |> List.mapi(fun idx (i, p, v) -> if idx = nindex then ((if i = "jmp" then "nop" else "jmp"), p, v) else (i, p, v)) |> List.ofSeq
        let nmemory = runCpu {memory with RAM.Instructions = ninstructions}
        
        if nmemory.Terminated then nmemory
        else replaceRamValues memory nindex
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Seq.map(fun l -> l.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)) |> Seq.map(fun l -> (l.[0], int(l.[1]), false)) |> List.ofSeq
        let ram = {RAM.Instructions = lines; RAM.Pointer = 0; RAM.Accumulator = 0L; RAM.Terminated = false}

        let ram1 = runCpu ram
        printfn "Accumulator value at the first infinite loop: %d" ram1.Accumulator

        let ram2 = replaceRamValues ram 0
        printfn "Accumulator value after replacing instructions: %d" ram2.Accumulator