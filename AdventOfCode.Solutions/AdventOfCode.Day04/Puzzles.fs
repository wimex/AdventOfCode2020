namespace AdventOfCode.Day04

module Puzzles =
    let describe = 
        "Passport Processing"
    
    let validateYearNumber (value: string) min max =
        let (parsed, year) = System.Int32.TryParse value
        parsed && (value.Length = 4 && min <= year && year <= max)

    let validateHeightNumber (value: string) =
        let number = value.Substring(0, value.Length - 2)
        let unit = value.Substring(number.Length, 2)
        let (parsed, length) = System.Int32.TryParse number

        match unit with
        | "cm" -> parsed && ((150 <= length) && (length <= 193))
        | "in" -> parsed && ((59 <= length) && (length <= 76))
        |    _ -> false

    let validateHairColor (value: string) = 
        let regex = new System.Text.RegularExpressions.Regex("^#[0-9a-f]{6}$")
        regex.Match(value).Success

    let validateEyeColor (value: string) =
        ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"] |> Seq.contains value

    let validatePassportId (value: string) =
        let regex = new System.Text.RegularExpressions.Regex("^[0-9]{9}$")
        regex.Match(value).Success

    let rec strictValidation (fields: (string * string) list) valid = 
        if not valid || fields.IsEmpty then valid
        else
            let (field, value) = fields.Head

            let fvalid =
                match field with
                | "byr" -> validateYearNumber value 1920 2002
                | "iyr" -> validateYearNumber value 2010 2020
                | "eyr" -> validateYearNumber value 2020 2030
                | "hgt" -> validateHeightNumber value
                | "hcl" -> validateHairColor value
                | "ecl" -> validateEyeColor value
                | "pid" -> validatePassportId value
                | "cid" -> true
                |     _ -> false

            strictValidation fields.Tail (valid && fvalid)

    let rec checkPassport (passports: string list) puzzle1 puzzle2 =
        if passports.IsEmpty then (puzzle1, puzzle2)
        else
            let regex = new System.Text.RegularExpressions.Regex("([A-z]+):([A-z0-9#]+)")
            let fields = regex.Matches(passports.Head)
                            |> List.ofSeq
                            |> List.map(fun l -> (l.Groups.[1].Value, l.Groups.[2].Value))
            
            let npuzzle1 = if fields.Length > 7 || fields.Length = 7 && not (fields |> List.map(fun (f, _) -> f) |> List.contains("cid")) then puzzle1 + 1 else puzzle1
            let npuzzle2 = if npuzzle1 > puzzle1 && strictValidation fields true then puzzle2 + 1 else puzzle2

            checkPassport passports.Tail npuzzle1 npuzzle2
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllText(filenames.Head)
        let passports = lines.Split("\n\n", System.StringSplitOptions.RemoveEmptyEntries) |> List.ofSeq |> List.map(fun l -> l.Replace('\n', ' '))
                            
        let puzzle1, puzzle2 = checkPassport passports 0 0
        
        printfn "Valid passports for puzzle 1: %d" puzzle1
        printfn "Valid passports for puzzle 2: %d" puzzle2
