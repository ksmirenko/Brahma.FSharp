module AlgorithmCYK.UserPart

open AlgorithmCYK
open System

let rulesInput() = 
    printfn "%s" "Enter a context-free grammar in Chomsky normal form: \n"
    let rec rulesInput list (set : Set<string>) =
        printfn "%s" "Enter a left part of the rule:  "
        let lp = Console.ReadLine()
        set.Add(lp) |> ignore
        printfn "%s" "Enter a right part of the rule:  "
        let rp = Console.ReadLine()
        printfn "%s" "Press ENTER to continue or SPACE to end."
        let rk = Console.ReadKey()
        if (rk.Key).ToString() = "Spacebar"
        then (lp,rp) :: list, set
        else rulesInput ((lp,rp) :: list) set
    rulesInput [] Set.empty   

let mainCYK() = 
    printfn "%s" "Enter a context-free grammar in Chomsky normal form ('S' is a start symbol): \n"
    printfn "%s" "Enter a start symbol of the grammar:  "
    let s = Console.ReadLine()
    let (rul, nonterm) = rulesInput()
    printfn "%s" "Enter a string for the CYK algorithm: \n"
    let str = Console.ReadLine()
    if str.Length = 0 
    then printfn "%s" "The string's lenght < 1. "
    elif (CYK rul (Set.toArray(nonterm)) str s)
    then printfn "%s" "The string can be generated. "
    else printfn "%s" "The string cannot be generated. "
    Console.ReadKey(true) |> ignore

mainCYK()