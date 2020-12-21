namespace AdventOfCode.Day21

open System.Linq

module Puzzles =
    let describe =
        "Allergen Assessment"

    let rec getComponents (lines: (string list * string list) list) (ingredients: string list) (allergens: string list) =
        if lines.IsEmpty then (ingredients |> List.distinct, allergens |> List.distinct)
        else getComponents lines.Tail (ingredients |> List.append (lines.Head |> fst)) (allergens |> List.append (lines.Head |> snd))

    let rec getMappings (commons: (string * string list) list) (mappings: (string * string) list) =
        if commons.IsEmpty then mappings |> List.sortBy(fun (a, _) -> a) |> List.ofSeq
        else
            let singles = commons |> List.where(fun (_, i) -> i.Length = 1)
            if singles.IsEmpty then raise(System.Exception("Unable to continue search"))
            else
                let nmappings = singles
                                    |> List.map(fun (a, i) -> (a, i.Head)) 
                                    |> List.append mappings
                let ncommons = commons //Remove already mapped ingredients and allergenes
                                    |> List.where(fun (a, _) -> not (nmappings |> List.exists(fun (x, _) -> x = a)))
                                    |> List.map(fun (a, i) -> (a, i |> List.where(fun x -> not (nmappings |> List.map(fun (_, s) -> s) |> List.exists(fun s -> x = s))) |> List.ofSeq))
                                    |> List.ofSeq

                getMappings ncommons nmappings
            

    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)
                        |> Seq.map(fun s -> s.Split("(contains ", System.StringSplitOptions.RemoveEmptyEntries))
                        |> Seq.map(fun r -> (r.[0].Trim().Split(" ", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq, r.[1].Trim().Trim('(', ')').Split(", ", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq))
                        |> List.ofSeq

        let (ingredients, allergens) = getComponents lines List.empty List.empty
        let repeats = lines //Create pairs and collect the ingredients that appear on multiple lists
                        |> List.allPairs lines 
                        |> List.map(fun ((i1, a1), (i2, a2)) -> (i1.Intersect(i2) |> List.ofSeq, a1.Intersect(a2) |> List.ofSeq)) 
                        |> List.where(fun (_, a) -> not (a.Length = 0)) 
                        |> List.ofSeq

        let commons = allergens //For every allergen, collect ingredients that appear every time
                        |> List.map(fun a -> (a, repeats |> List.where(fun (_, v) -> v |> List.contains(a)) |> List.map(fun (i, _) -> i) |> List.reduce(fun s l -> (l.Intersect(s) |> List.ofSeq))))
                        |> List.ofSeq

        let found = commons //Select elements where there is information about them
                        |> List.map(fun (_, i) -> i) 
                        |> List.reduce(fun s i -> List.append i s) 
                        |> List.distinct 
                        |> List.ofSeq
        
        let notfound = ingredients //Select elements that are not on the found list
                        |> List.where(fun i -> not(found |> List.contains i)) 
                        |> List.ofSeq
                        
        let occurs = notfound //Calculate number of occurences
                        |> List.map(fun i -> lines 
                                                |> List.map(fun (x, _) -> x) 
                                                |> List.map(fun x -> x |> List.where(fun f -> f = i) |> List.length) 
                                                |> List.sum) 
                        |> List.sum

        let mappings = getMappings commons List.empty
        
        printfn "Number of appearances %d" occurs
        printfn "Dangerous ingredient list: %s" (mappings |> List.map(fun (_, i) -> i) |> String.concat(","))
