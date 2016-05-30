module AlgorithmCYK.ParallelCPU
open AlgorithmCYK
open System.Threading.Tasks

let matrixCYKParallelCPU (rules : list<string * string>) (nonterm : string array) (str : string) (n : int) (matrix : int array [][]) = 
    Parallel.For(0, n, fun k -> allRulesCheck rules nonterm matrix.[n - 1].[k] [|(str.[k].ToString())|]) |> ignore
    for l = 1 to n - 1 do
        for i = 0 to n - l - 1 do
            Parallel.For(0, l, fun j -> allRulesCheck rules nonterm matrix.[n - 1 - l].[i] (compCreate nonterm matrix.[n - 1 - j].[i] matrix.[j + n - l].[i + j + 1]))
            |> ignore
    matrix

let CYKParallelCPU (rules : list<string * string>) (nonterm : string array) (str : string) (start : string) = 
    if str = ""
    then true
    else 
        let n = str.Length 
        let matrix =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate nonterm.Length|]|]
        let matrCYK = matrixCYKParallelCPU rules nonterm str n matrix
        conclCYK matrCYK nonterm start