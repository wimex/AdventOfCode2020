namespace AdventOfCode.Day14

open System
open System.Text.RegularExpressions

module Puzzles =
    let describe =
        "Docking Data"

    let getMask value =
        value |> List.ofSeq

    let getValue1 mask value =
        match mask with
        | 'X' -> value
        | '0' -> '0'
        | '1' -> '1'
        |   _ -> raise(Exception("Invalid mask value"))

    let setValue1 saddress svalue mask (memory: Map<int64, int64>)=
        let value = Convert.ToString(int(svalue), 2).PadLeft(36, '0') |> List.ofSeq
        let result = value |> List.zip mask |> List.map(fun (m, v) -> getValue1 m v) |> List.ofSeq

        let address = int64(saddress)
        let nmemory = memory.Add(address, Convert.ToInt64(String.Join("", result), 2))
        nmemory
        
    let rec getValue2 mask value =
        match mask with
        | '0' -> value
        | '1' -> '1'
        | 'X' -> 'X'
        |   _ -> raise(Exception("Invalid mask value"))
        
    let rec getAddresses2 (mask: char list) (results: char list) =
        if mask.IsEmpty then results
        else
            let head = mask.Head
            if head = 'X' then
                let left = getAddresses2 mask.Tail (results @ ['0'])
                let right = getAddresses2 mask.Tail (results @ ['1'])
                left @ right
            else getAddresses2 mask.Tail (results @ [head])

    let rec setValue2x (addresses: int64 list) value (memory: Map<int64, int64>) =
        if addresses.IsEmpty then memory
        else
            let address = addresses.Head
            let nmemory = memory.Add(address, value)
            setValue2x addresses.Tail value nmemory

    let setValue2 saddress svalue mask (memory: Map<int64, int64>) =
        let address = Convert.ToString(int(saddress), 2).PadLeft(36, '0') |> Seq.zip mask |> Seq.map(fun (m, v)-> getValue2 m v) |> List.ofSeq
        let addresses = getAddresses2 address List.empty
        let pieces = addresses |> Seq.chunkBySize 36 |> Seq.map(fun x -> Convert.ToInt64(String.Join("", x), 2)) |> List.ofSeq
        
        let value = int64(svalue)
        let nmemory = setValue2x pieces value memory
        nmemory

    let setValue version address value mask memory =
        if version = 1 then setValue1 address value mask memory
        else setValue2 address value mask memory

    let rec writeMemory version (instructions: (string * string) list) mask (memory: Map<int64, int64>) =
        if instructions.IsEmpty then memory
        else
            let (instruction, value) = instructions.Head
            let nmask = if instruction = "mask" then getMask value else mask
            let nmemory = if instruction = "mask" then memory else setValue version instruction value mask memory

            writeMemory version instructions.Tail nmask nmemory

    let parseLine (line: string) =
        let rmem = new Regex("mem\[(\d+)\] = (\d+)");
        let rmsk = new Regex("mask = ([01X]+)");
        
        if line.StartsWith("mem") then
            let matches = rmem.Match(line);
            (matches.Groups.[1].Value, matches.Groups.[2].Value)
        else
            let matches = rmsk.Match(line);
            ("mask", matches.Groups.[1].Value)

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Seq.map(fun x -> parseLine x) |> List.ofSeq

        let memory1 = writeMemory 1 lines List.empty Map.empty
        let sum1 = memory1 |> Seq.sumBy(fun item -> item.Value)
        printfn "Sum of memory contents v1: %d" sum1

        let memory2 = writeMemory 2 lines List.empty Map.empty
        let sum2 = memory2 |> Seq.sumBy(fun item -> item.Value)
        printfn "Sum of memory contents v2: %d" sum2