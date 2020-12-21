namespace AdventOfCode.Day20

open System.Linq

module Puzzles =
    let describe = 
        "Jurassic Jigsaw"

    let monster = 
        "                  # " + 
        "#    ##    ##    ###" +
        "#  #  #  #  #  #    "

    type Hash = string * string

    type Tile = 
        {
            data: char[,]
            hashes: string list
            neighbors: (int64 * string list) list
        }

    let getHash (array: char[]) =
        let part1 = array |> Array.map(fun f -> string(f)) |> String.concat ""
        let part2 = array |> Array.rev |> Array.map(fun f -> string(f)) |> String.concat ""
        (part1, part2)
 
    let chunkToTile (chunk: string[]) =
        let name = int64(chunk.[0].Substring(chunk.[0].Length - 5, 4))
        let data = Array2D.init 10 10 (fun x y -> chunk.[y + 1].[x])

        let (hash1a, hash1b) = getHash (Array.init 10 (fun x -> data.[x, 0]))
        let (hash2a, hash2b) = getHash (Array.init 10 (fun x -> data.[9, x]))
        let (hash3a, hash3b) = getHash (Array.init 10 (fun x -> data.[x, 9]))
        let (hash4a, hash4b) = getHash (Array.init 10 (fun x -> data.[0, x]))

        (name, {data = data; hashes = [hash1a; hash1b; hash2a; hash2b; hash3a; hash3b; hash4a; hash4b]; neighbors = List.empty})

    let findNeighbors (n: int64) (tiles: Map<int64, Tile>) =
        let tile = tiles.[n]
        let hashes = tile.hashes
        let all = tiles |> Seq.map(fun (KeyValue(n, t)) -> (n, (t.hashes.Intersect(hashes) |> List.ofSeq))) |> Seq.where(fun (m, h) -> m <> n && h.Length > 0) |> List.ofSeq
        {tile with neighbors = all}

    //let rec arrangeTiles (x, y) (tiles: Map<int64, Tile>) (corners: (int64 * Tile) list) (map: Tile[,]) =
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Seq.chunkBySize(12) |> List.ofSeq
        let tiles = lines |> List.map(fun chunk -> chunkToTile chunk) |> Map.ofSeq
        let neighbors = tiles |> Map.map(fun n _ -> findNeighbors n tiles)

        let corners = neighbors |> Seq.where(fun (KeyValue(_, v)) -> v.neighbors.Length = 2) |> Seq.map(fun (KeyValue(k, v)) -> (k, v)) |> List.ofSeq
        printfn "Sum of corner tile IDs %d" (corners |> List.map(fun (k, _) -> k) |> List.fold (*) 1L)

        let mapsize = int(sqrt(double(tiles.Count)))
        //let tilemap = arrangeTiles tiles corners (Array2D.zeroCreate<Tile> mapsize mapsize)
        
        printfn "Bye"
