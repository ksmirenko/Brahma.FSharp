module AlgorithmCYK
open System

let rulesInput() = 
    let array = [||]
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
    let len = comp.Length
    let arr = ResizeArray<string>()
    let n = rules.Length
    for j in 0..len - 1 do
        for i in 0..n - 1 do
           arr.AddRange(oneRuleCheck rules.[i] comp.[j])
    arr.ToArray()

let deleteEqual (arr : array<'t>) = 
    match arr with 
    | [||] -> arr
    | _ ->   
        let n = arr.Length
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
    let n1 = arr1.Length
    let n2 = arr2.Length
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
 
let conclCYK (matrix : string array [][]) (start : string) = 
    let rec concCYK (arr : string array) (i : int) = 
        if i <> Array.length(arr) 
        then
            if arr.[i] = start 
            then "The string can be generated. "
            else concCYK arr (i + 1)
        else "The string cannot be generated. "
    concCYK matrix.[0].[0] 0

let CYK (rules : (string*string) []) (str : string) (start : string) = 
    let n = str.Length
    if n <> 0 
    then
        let matrix  : string array [][] =  [| for i in 0 .. n - 1 do yield [| for j in 0 .. n - 1 do yield [|""|] |] |]
        let matrCYK = matrixCYK rules str n matrix
        conclCYK matrCYK start
    else "The string's lenght < 1. "
 
let mainCYK() = 
    printfn "%s" "Enter a context-free grammar in Chomsky normal form ('S' is a start symbol): \n"
    printfn "%s" "Enter a start symbol of the grammar:  "
    let s = Console.ReadLine()
    let rules = rulesInput()
    printfn "%s" "Enter a string for the CYK algorithm: \n"
    let str = Console.ReadLine()
    printfn "%s" (CYK rules str s)
    Console.ReadKey(true) |> ignore

mainCYK()

