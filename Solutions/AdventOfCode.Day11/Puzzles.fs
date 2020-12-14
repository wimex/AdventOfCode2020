namespace AdventOfCode.Day11

module Puzzles =
    let describe =
        "Seating System"

    type Directions =
        static member data = [-1; 0; 1] |> List.allPairs [-1; 0; 1] |> List.where(fun (x, y) -> not (x = 0 && y = 0)) |> List.ofSeq

    let rec checkSight version (field: char[,]) (px, py) (sx, sy) =
        let w = field.GetLength 0
        let h = field.GetLength 1

        let nx = px + sx
        let ny = py + sy

        if nx < 0 || nx >= w || ny < 0 || ny >= h then false
        else if field.[nx, ny] = 'L' then false
        else if field.[nx, ny] = '#' then true
        else 
            if version = 0 then false
            else checkSight version field (nx, ny) (sx, sy)

    let updateSquare version (field: char[,]) x y =
        let current = field.[x,y]
        if current = '.' then current
        else
            let target = if version = 0 then 4 else 5
            let count = Directions.data 
                            |> Seq.map(fun (dx, dy) -> checkSight version field (x, y) (dx, dy))
                            |> Seq.where(fun x -> x)
                            |> Seq.length

            if current = 'L' && count = 0 then '#'
            else if current = '#' && count >= target then 'L'
            else current

    let rec updateField version (field: char [,]) changed =
        if not changed then field
        else
            let nfield = field |> Array2D.mapi(fun x y _ -> updateSquare version field x y)
            let nchanged = field |> Array2D.mapi(fun x y v -> (nfield.[x,y] <> v)) |> Seq.cast<bool> |> Seq.where(fun x -> x) |> Seq.length > 0
            updateField version nfield nchanged

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)
        let field = Array2D.init lines.[0].Length lines.Length (fun x y -> lines.[y].[x])

        let result1 = updateField 0 field true
        let result2 = updateField 1 field true

        let occupied1 = result1 |> Array2D.map(fun v -> v = '#') |> Seq.cast<bool> |> Seq.where(fun x -> x) |> Seq.length
        let occupied2 = result2 |> Array2D.map(fun v -> v = '#') |> Seq.cast<bool> |> Seq.where(fun x -> x) |> Seq.length

        printfn "Occupied seats 1: %d" occupied1
        printfn "Occupied seats 2: %d" occupied2