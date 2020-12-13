namespace AdventOfCode.Day13

module Puzzles =
    let describe =
        "Shuttle Search"

    let rec findNextDeparture (timestamp: bigint) (departures: bigint list) =
        let remainders = departures |> List.map(fun i -> (i, timestamp % i))
        let valids = remainders |> List.where(fun (_, v) -> v = 0I) |> List.map(fun (r, _) -> r)

        if not valids.IsEmpty then (timestamp, valids.Head)
        else findNextDeparture (timestamp + 1I) departures

    let rec gcdStep (n1r, n1s, n1t) (n2r, n2s, n2t) =
        if n2r = 0I then (n1r, n1s, n1t)
        else
            let q = n1r / n2r
            let r = n1r - q*n2r
            let s = n1s - q*n2s
            let t = n1t - q*n2t
            gcdStep (n2r, n2s, n2t) (r, s, t)

    //Extended Eucledian Alorithm
    //num1x + num2y = gcd(num1, num2) where x = s and y = t
    let gcd (num1: bigint) (num2: bigint) =
        gcdStep (num1, 1I, 0I) (num2, 0I, 1I)

    //This is a simple implementation of the Chinese Remainder Theorem
    //Here is a great explanation: https://www.youtube.com/watch?v=zIFehsBHB8o
    let remainders modulos equals =
        let nsum = modulos |> List.fold (*) 1I
        let nis = modulos |> List.zip equals |> List.map(fun (b, m) -> (m, b, nsum / m))
        let sum = nis
                    |> List.map(fun (m, b, n) -> (m, b, n, gcd n m))            //n*x ≡ 1 (mod m)
                    |> List.map(fun (m, b, n, (_, x, _)) -> (m, b, n, m + x))   //gcd returns (1, s, t) we extract m + s
                    |> List.map(fun (_, b, n, x) -> b * n * x)                  //bi * ni * xi
                    |> List.sum                                                 //E(bi * Ni * xi)

        (sum, nsum)

    let rec reduceresult (number, modulo) limit =
        if number < modulo || (number - modulo) < limit then number
        else reduceresult ((number - modulo), modulo) limit

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head) |> Array.ofSeq
        let after = bigint(int(lines.[0]))
        let departures = lines.[1].Split(",", System.StringSplitOptions.RemoveEmptyEntries) |> Array.where(fun x -> x <> "x") |> Array.map(fun x -> bigint(int(x))) |> List.ofSeq

        let (timestamp1, bus) = findNextDeparture after departures
        printfn "Found next departure: bus %A @ %A -> %A" bus timestamp1 ((timestamp1 - after) * bus)

        //For the second part, the file contains values n1,n2,... nx and we are basically looking for numbers where x ≡ 0 (mod n1), x + 1 ≡ 0 (mod n2), ... x + n ≡ 0 (mod nx)
        let contest = lines.[1].Split(",", System.StringSplitOptions.RemoveEmptyEntries)
        let parsed = contest |> Seq.mapi(fun index value -> (value, index)) |> Seq.where(fun (value, _) -> not (value = "x")) |> Seq.map(fun(value, index) -> (bigint(int(value)), index))
        
        //After parsing the file, the first list contains modulo values (n1 ... nx)
        //and the second list contains the right part of the equation rewritten as nx - n e.g x + 2 ≡ 0 (mod n3) becomes x ≡ (n3 - 2) (mod n3)
        let modulos = parsed |> Seq.map(fun (modulo, _) -> modulo) |> List.ofSeq
        let equals = parsed |> Seq.map(fun (modulo, index) -> modulo - bigint(index)) |> List.ofSeq

        //We can apply the Chinese Remainder Theorem to these numbers because everything has been rearranged to contain only x
        let (number, modulo) = remainders modulos equals
        let reduced = reduceresult (number, modulo) 100000000000000I
        printfn "Found timestamp: %A" reduced