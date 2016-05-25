module AlgorithmCYK
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
        
let allRulesCheck (rules : list<string * string>) (nonterm : string array) (ntgn : int array) (comp : string array) = 
    for i = 0 to comp.Length - 1 do
        for j = 0 to rules.Length - 1 do
            match rules.[j] with 
            | (ch, str) -> if comp.[i] = str
                           then
                               for k = 0 to nonterm.Length - 1 do
                                   if nonterm.[k] = ch
                                   then ntgn.[k] <- 1


let compCreate (nonterm : string array) (ntgen1 : int array) (ntgen2 : int array) =
    let comp = ResizeArray<string>() 
    for i = 0 to ntgen1.Length - 1 do
        if ntgen1.[i] = 1
        then
            for j = 0 to ntgen2.Length - 1 do
                if ntgen2.[i] = 1
                then comp.Add(nonterm.[i] + nonterm.[j])
    comp.ToArray()
              
let matrixCYK (rules : list<string * string>) (nonterm : string array) (str : string) (n : int) (matrix : int array [][]) = 
    for k = 0 to n - 1 do 
        allRulesCheck rules nonterm matrix.[n - 1].[k] [|(str.[k].ToString())|]
    printfn "%A" matrix
    for l = 1 to n - 1 do
        for i = 0 to n - l - 1 do
            for j = 0 to l - 1 do
                let x = compCreate nonterm matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1]
                allRulesCheck rules nonterm matrix.[n - 1 - l].[i] x
    matrix

let conclCYK (matrix : int array [][]) (nonterm : string array) (start : string) = 
    let mutable ind = -1
    for i = 0 to nonterm.Length - 1 do
        if nonterm.[i] = start
        then ind <- i
    if matrix.[0].[0].[ind] = 1
    then true
    else false

let CYK (rules : list<string*string>) (nonterm : string array) (str : string) (start : string) = 
    if str = ""
    then true
    else 
        let n = str.Length 
        let ntgen = Array.zeroCreate (nonterm.Length)
        let matrix  : int array [][] =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> ntgen|] |]
        printfn "%A" matrix
        let matrCYK = matrixCYK rules nonterm str n matrix
        conclCYK matrCYK nonterm start
        
let result = CYK [("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")] [|"S";"A";"B";"C"|] "baaba" "S"
printfn "%b" result

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

Console.ReadKey(true) |> ignore
//mainCYK()

