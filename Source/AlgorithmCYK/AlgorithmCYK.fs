module AlgorithmCYK
open System
 
let oneRuleCheck ((ch : string), (str : string)) (nonterm : string array) (ntgn : int array) (comp : string) = 
    if comp = str
    then 
        for i = 0 to nonterm.Length - 1 do
            if nonterm.[i] = ch
            then ntgn.[i] <- 1
       
let allRulesCheck (rules : list<string * string>) (nonterm : string array) (ntgn : int array) (comp : string array) = 
    for i = 0 to comp.Length - 1 do
        for j = 0 to rules.Length - 1 do
            oneRuleCheck rules.[j] nonterm ntgn  comp.[i]

let compCreate (nonterm : string array) (ntgen1 : int array) (ntgen2 : int array) =
    let comp = ResizeArray<string>() 
    for i = 0 to ntgen1.Length - 1 do
        if ntgen1.[i] = 1
        then
            for j = 0 to ntgen2.Length - 1 do
                if ntgen2.[j] = 1
                then comp.Add(nonterm.[i] + nonterm.[j])
    comp.ToArray()
              
let matrixCYK (rules : list<string * string>) (nonterm : string array) (str : string) (n : int) (matrix : int array [][]) = 
    for k = 0 to n - 1 do 
        allRulesCheck rules nonterm matrix.[n - 1].[k] [|(str.[k].ToString())|]
    for l = 1 to n - 1 do
        for i = 0 to n - l - 1 do
            for j = 0 to l - 1 do
                allRulesCheck rules nonterm matrix.[n - 1 - l].[i] (compCreate nonterm matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1])
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
        let matrix  : int array [][] =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate (nonterm.Length)|]|]
        printfn "%A" matrix
        let matrCYK = matrixCYK rules nonterm str n matrix
        conclCYK matrCYK nonterm start