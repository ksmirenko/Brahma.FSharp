module AlgorithmCYK.ForGraphs
open AlgorithmCYK

let diagonalMatrixCYK (begtRules : array<int>) (endtRules : array<int>) (begnRules : array<int>) (endnRules : array<int>*array<int>) (str : array<int>) n (matrix : array<array<array<int>>>) = 
    for k = 0 to n - 1 do 
        rulesCheck str.[k] begtRules endtRules matrix.[n - 1].[k] 
    for l = 1 to n - 1 do
        for i = 0 to n - l - 1 do
            for j = 0 to l - 1 do
                if n - l - 1 = i 
                then compCheck begnRules endnRules matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1] matrix.[n - 1 - l].[i]
    matrix

let matrixShift (mtrx : array<array<array<int>>>) =
    let len = mtrx.Length
    let save = [|for i in 0..len - 1 -> [|for j in 0..len - 1 -> Array.zeroCreate (mtrx.[0].[0]).Length|]|]
    for i = 0 to len - 1 do
        for j = 1 to len - 1 do
        save.[i].[j - 1] <- mtrx.[i].[j]
    save 

let searchMtrx (mtrx : array<array<array<int>>>) ind n k (str : string) =
    let arr = ResizeArray<string>()
    for i = n - 1 to k - 1 do
        if mtrx.[i].[0].[ind] = 1
        then 
            arr.Add(str.[0..i + 1])
    arr

let strInRange (str : string) n k (term, nonterm, begnRules, endnRules, begtRules, endtRules) (start : string) = 
    let arrOfStr = ResizeArray<string>()
    let ind = findIndex nonterm start
    let len = str.Length
    let matrix =  [|for i in 0..k - 1 -> [|for j in 0..k - 1 -> Array.zeroCreate nonterm.Length|]|]
    let mutable res = matrixCYK begtRules endtRules begnRules endnRules (formStr term str.[0..k - 1]) k matrix
    arrOfStr.AddRange(searchMtrx res ind n k str.[0..k - 1])
    let save = matrixShift res
    for i = 1 to len - k - 1 do
        res <- diagonalMatrixCYK begtRules endtRules begnRules endnRules (formStr term str.[i..i + k - 1]) k save
        arrOfStr.AddRange(searchMtrx res ind n k str.[i..i + k - 1])
        res <- matrixShift res
    arrOfStr 

let search (mtrx : array<array<string>>) n k (rules : array<string * string>) (nonterm : array<string>) (start : string) = 
    let (term, nonterm, begnRules, endnRules, begtRules, endtRules) = formRules rules
    let rl = (term, nonterm, begnRules, endnRules, begtRules, endtRules)
    let visited = Array.zeroCreate (mtrx.[0].Length)
    let arr = ref (ResizeArray<string>())
    let rec search a b n k (inWork : string) = 
        if a <> b 
        then 
            for i = 0 to b do
                if mtrx.[a].[i] <> "" && visited.[i] = 0
                then 
                    visited.[i] <- 1
                    let len = (inWork + mtrx.[a].[i]).Length
                    if len < k
                    then 
                        search i b n k (inWork + mtrx.[a].[i]) 
                    else 
                        (!arr).AddRange(strInRange (inWork + mtrx.[a].[i]) n k rl start)
                        search i b n k (inWork + mtrx.[a].[i]).[len - k..len - 1]
                    visited.[i] <- 0
        else 
            if inWork.Length >= n
            then (!arr).AddRange(strInRange inWork n inWork.Length rl start)
    search 0 (mtrx.[0].Length - 1) n k ""
    (!arr).ToArray()