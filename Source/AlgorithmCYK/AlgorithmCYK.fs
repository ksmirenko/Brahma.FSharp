module AlgorithmCYK
open System

let rulesInput = 
    let array = [||]
    printfn "%s" "Enter a context-free grammar in Chomsky normal form ('S' is a start symbol): \n"
    let rec rulesInput array =
        printfn "%s" "Enter a left part of the rule:  "
        let lp = Console.ReadLine()
        printfn "%s" "Enter a right part of the rule:  "
        let rp = Console.ReadLine()
        printfn "%s" "Press ENTER to continue or SPACE to end."
        let rk = Console.ReadKey()
        if (rk.Key).ToString() = "Spacebar"
        then Array.append array [|(lp,rp)|]
        else rulesInput (Array.append array [|(lp,rp)|])
    rulesInput array

let oneRuleCheck ((ch : string),(str: string)) (comp : string) =
    if comp = str 
    then [|ch|]
    else [||]

let allRulesCheck (rules : array<string * string>) (comp : string array) = 
    let len = Array.length(comp)
    let arr = ResizeArray<string>()
    let n = Array.length(rules)
    for j in 0..len - 1 do
        for i in 0..n - 1 do
           arr.AddRange(oneRuleCheck rules.[i] comp.[j])
    arr.ToArray()

let deleteEqual (arr : array<'t>) = 
    match arr with 
    | [||] -> arr
    | _ ->   
        let n = Array.length(arr)
        let res = ResizeArray<'t>()
        res.Add(arr.[0])
        for i = 1 to n - 1 do
            let mutable f = 0
            let reslen = res.Count
            for j = 0 to reslen - 1 do
                if arr.[i] = res.[j]
                then f <- 1
            if f = 0
            then 
                res.Add(arr.[i])
        res.ToArray()

let decMultiply (arr1 : array<string>) (arr2 : array<string>) =
    let res = ResizeArray<string>()
    let n1 = Array.length(arr1)
    let n2 = Array.length(arr2)
    match n1, n2 with
    | 0, n2 -> arr2
    | n1, 0 -> arr1
    | _, _ -> for i in 0..n1 - 1 do
                  for j in 0..n2 - 1 do
                      res.Add(arr1.[i] + arr2.[j])
              res.ToArray()

let matrixCYK (rules : array<string * string>) (str: string) (n: int) (matrix  : string array [][]) = 
    let dm = ResizeArray<string>() 
    for i = 0 to n - 1 do 
        matrix.[n - 1].[i] <- allRulesCheck rules [|(str.[i].ToString())|]
    for l = 1 to n - 1 do
        for i = 0 to n - l - 1 do
            for j = 0 to l - 1 do
                dm.AddRange(decMultiply matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1])
                matrix.[n - 1 - l].[i]  <- deleteEqual (Array.append matrix.[n - 1 - l].[i] (allRulesCheck rules (dm.ToArray())))
                dm.Clear()
    matrix
 
let conclCYK (matrix : string array [][]) = 
    let rec concCYK (arr : string array) (f : bool) (i : int) = 
        match f with 
        | true -> "The string can be generated. "
        | false -> 
                    if i <> Array.length(arr) 
                    then
                        if arr.[i] = "S" 
                        then concCYK arr true i
                        else concCYK arr f (i + 1)
                    else "The string cannot be generated. "
    concCYK matrix.[0].[0] false 0

let CYK (rules : (string*string) []) (str : string) = 
    let n = str.Length
    if n > 1 
    then
        let matrix  : string array [][] =  [| for i in 0 .. n - 1 do yield [| for j in 0 .. n - 1 do yield [|""|] |] |]
        let matrCYK = matrixCYK rules str n matrix
        conclCYK matrCYK
    else "The string's lenght < 1. "
 
let mainCYK = 
    let rules = rulesInput
    printfn "%s" "Enter a string for the CYK algorithm: \n"
    let str = Console.ReadLine()
    CYK rules str
    
printfn "%s" mainCYK

let rk = Console.ReadKey(true)