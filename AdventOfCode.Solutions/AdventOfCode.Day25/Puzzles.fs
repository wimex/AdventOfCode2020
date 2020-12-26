namespace AdventOfCode.Day25

module Puzzles =
    let describe =
        "Combo Breaker"

    let divider = 20201227L
    
    let rec calc (subject: int64) (target: int64) (value: int64) =
        if target = 0L then value
        else
            let multiplied = value * subject
            let nvalue = multiplied % divider
            calc subject (target - 1L) nvalue

    let rec guess (subject: int64) (target: int64) (value: int64) (counter: int64) =
        let nvalue = calc subject 1L value

        if nvalue = target then counter
        else guess subject target nvalue (counter + 1L)
        
    let execute (filenames: string list) =
        let lines = System.IO.File.ReadAllLines(filenames.Head)

        let subject = 7L
        let pubkey1 = int64(lines.[0])
        let pubkey2 = int64(lines.[1])

        let loop1 = guess subject pubkey1 1L 1L
        let loop2 = guess subject pubkey2 1L 1L

        let privkey1 = calc pubkey2 loop1 1L
        let privkey2 = calc pubkey1 loop2 1L
        
        printfn "The door's private key is %d = %d" privkey1 privkey2