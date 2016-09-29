module AlgorithmCYK.ParallelGPGPU

open Brahma.OpenCL
open OpenCL.Net
open Brahma.FSharp.OpenCL.Translator
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Microsoft.FSharp.Quotations
open AlgorithmCYK

let rows (arr : array<_>) = arr.Length
let cols (arr : array<array<_>>) = arr.[0].Length
let toArr arr = Array.concat(arr)
let toMtrx (arr : array<_>) rows cols = Array.init rows (fun i -> Array.init cols (fun j -> arr.[cols * i + j]))
let arrSum (arr1 : array<int>) (arr2 : array<int>) = 
    for i = 0 to arr1.Length - 1 do
        if arr1.[i] + arr2.[i] > 0
        then arr2.[i] <- 1
    arr2
    
let mtrxSum (mtrx1 : array<array<int>>) (mtrx2 : array<array<int>>) =
    for i = 0 to mtrx1.Length - 1 do
        mtrx2.[i] <- arrSum mtrx1.[i] mtrx2.[i]
    mtrx2

let provAndQueue() = 
    let platformName = "*"
    let deviceType = DeviceType.Default
    let provider =
        try  ComputeProvider.Create(platformName, deviceType)
        with 
        | ex -> failwith ex.Message
    let commandQueue = new Brahma.OpenCL.CommandQueue(provider, provider.Devices |> Seq.head)
    provider, commandQueue

let result (mtrxel : array<_>) rows cols (provider: ComputeProvider) (commandQueue: Brahma.OpenCL.CommandQueue) = 
    let _ = commandQueue.Add(mtrxel.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    toMtrx mtrxel rows cols

let rulesCheckParal (begtRules : array<int>) (endtRules : array<int>) (mtrxel : array<array<_>>) (str : array<int>) = 
    let rows = rows mtrxel
    let cols = cols mtrxel
    let (provider, commandQueue) = provAndQueue()
    let command = 
        <@ 
            fun (rng : _1D) (mtrxel : array<_>) (formStr : array<_>) lenR (endtR : array<_>) (begtR : array<_>) rows cols (toMtrxel : array<_>) ->
                let r = rng.GlobalID0
                for k = 0 to lenR - 1 do
                    if endtR.[k] = formStr.[r]
                    then 
                        toMtrxel.[r * cols + begtR.[k]] <- 1
        @>
    let length = str.Length 
    let localWorkSize = length
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d = new _1D(length, 1)
    let newMtrxel = toArr mtrxel
    let toMtrxel = Array.zeroCreate newMtrxel.Length
    kernelPrepare d newMtrxel str endtRules.Length endtRules begtRules rows cols toMtrxel
    let _ = commandQueue.Add(kernelRun())
    result toMtrxel rows cols provider commandQueue

let compCheckParal (begnRules : array<int>) (endnRulesL : array<int>) (endnRulesR : array<int>) (mtrxel1 : array<_>) (mtrxel2 : array<_>) (mtrxel3 : array<_>) a b =
    let rows = rows mtrxel3
    let cols = cols mtrxel3
    let (provider, commandQueue) = provAndQueue()
    let command = 
        <@
            fun (rng : _1D) (begnR : array<_>) (endnRL : array<_>) (endnRR : array<_>) (mtrxel1 : array<_>) (mtrxel2 : array<_>) (mtrxel3 : array<_>) lenEl lenR a b cols (toArr : array<_>) ->
                let r = rng.GlobalID0
                let cols1Shift = r * cols
                let cols2Shift = (r + b + 1) * cols
                for i = 0 to lenEl - 1 do
                    if mtrxel1.[cols1Shift + i] = 1
                    then 
                        for j = 0 to lenEl - 1 do
                            if mtrxel2.[cols2Shift + j] = 1
                            then
                                for k = 0 to lenR - 1 do
                                    if (i = endnRL.[k]) && (j = endnRR.[k])
                                    then toArr.[cols1Shift + begnR.[k]] <- 1     
        @>
    let newMtrxel = toArr mtrxel3
    let length = newMtrxel.Length 
    let localWorkSize = length
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d = new _1D(length, 1)
    let toMtrxel = Array.zeroCreate length
    kernelPrepare d begnRules endnRulesL endnRulesR mtrxel1 mtrxel2 newMtrxel length begnRules.Length a b cols toMtrxel
    let _ = commandQueue.Add(kernelRun())
    result toMtrxel rows cols provider commandQueue   

let matrixCYKParallelGPGPU (begtRules : array<int>) (endtRules : array<int>) (begnRules : array<int>) (endnRulesL : array<int>) (endnRulesR : array<int>) (str : array<int>) n (matrix : array<array<array<int>>>) = 
    matrix.[n - 1] <- rulesCheckParal begtRules endtRules matrix.[n - 1] str
    for a = 1 to n - 1 do
        for b = 0 to a - 1 do 
            matrix.[n - a - 1].[..n - a - 1] <- mtrxSum (compCheckParal begnRules endnRulesL endnRulesR (toArr matrix.[n - 1 - b]) (toArr matrix.[n - a + b]) matrix.[n - a - 1].[..n - a - 1] a b) matrix.[n - a - 1].[..n - a - 1]
    matrix

let CYKParallelGPGPU (rules : array<string*string>) str start = 
    if str = ""
    then true
    else 
        let (term, nonterm, begnRules, endnRules, begtRules, endtRules) = formRules rules
        let (endnRulesL, endnRulesR) = fst endnRules, snd endnRules
        let strArr = formStr term str
        let n = str.Length 
        let matrix =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate nonterm.Length|]|]
        let matrCYK = matrixCYKParallelGPGPU begtRules endtRules begnRules endnRulesL endnRulesR strArr n matrix
        conclCYK matrCYK nonterm start

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let allCYKGPU (meh : array<array<array<_>>>) nontermNumb (begnRules : array<int>) (endnRules : array<int> * array<int>) =
    let (endnRulesL, endnRulesR) = fst endnRules, snd endnRules
    let n = rows meh
    let arr = toArr (toArr meh)
    let l = arr.Length
    let (provider, commandQueue) = provAndQueue()
    let command = 
        <@ 
            fun (rng : _1D) (meh : array<int>) n nontermNumb (begnRules : array<int>) (endnRulesL : array<int>) enLen (endnRulesR : array<int>) (newMtrx : array<int>) l ->
                for a = 1 to n - 1 do
                    for b = 0 to a - 1 do 
                        let r = rng.GlobalID0                
                        if n - a >= r
                        then 
                            for i = 0 to nontermNumb - 1 do
                                if meh.[(n - 1 - b) * n * nontermNumb + r * nontermNumb + i] = 1
                                then 
                                    for j = 0 to nontermNumb - 1 do
                                        if meh.[(b + n - a) * n * nontermNumb + (b + r + 1) * nontermNumb + j] = 1
                                        then 
                                            for k = 0 to enLen - 1 do
                                                if (i = endnRulesL.[k]) && (j = endnRulesR.[k])
                                                then meh.[(n - 1 - a) * n * nontermNumb + r * nontermNumb + begnRules.[k]] <- 1
                for k = 0 to l - 1 do
                    newMtrx.[k] <- meh.[k]
        @>

    let rng = new _1D(n * n * nontermNumb, nontermNumb * n)
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let newMtrx = Array.zeroCreate l
    kernelPrepare rng arr n nontermNumb begnRules endnRulesL endnRulesR.Length endnRulesR newMtrx l
    let _ = commandQueue.Add(kernelRun())
    let _ = commandQueue.Add(newMtrx.ToHost provider).Finish()
    commandQueue.Dispose()
    provider.Dispose()
    provider.CloseAllBuffers()
    toMtrx newMtrx (n * n) nontermNumb

(*
let allCYKGPU (meh : array<array<array<_>>>) nontermNumb (begnRules : array<int>) (endnRules : array<int> * array<int>) =
    let (endnRulesL, endnRulesR) = fst endnRules, snd endnRules
    let rows = rows meh
    let cols = cols meh
    let n = rows
    let arr = toArr (toArr meh)
    let l = arr.Length
    let (provider, commandQueue) = provAndQueue()
    for a = 1 to n - 1 do
        for b = 0 to a - 1 do
            for c = 0 to n - 1 do
                if n - a - 1 >= c
                then 
                    for i = 0 to nontermNumb - 1 do
                        if arr.[(n - 1 - b) * n * nontermNumb + c * nontermNumb + i] = 1
                        then 
                            for j = 0 to nontermNumb - 1 do
                                if arr.[(b + n - a) * n * nontermNumb + (b + c + 1) * nontermNumb + j] = 1
                                then 
                                    for k = 0 to endnRulesR.Length - 1 do
                                        if (i = endnRulesL.[k]) && (j = endnRulesR.[k])
                                        then arr.[(n - 1 - a) * n * nontermNumb + c * nontermNumb + begnRules.[k]] <- 1
    let newMtrx = Array.zeroCreate l
    for k = 0 to l - 1 do
        newMtrx.[k] <- arr.[k]
    toMtrx newMtrx (rows * cols) nontermNumb
*)

let allCYKParallelGPGPU (begtRules : array<int>) (endtRules : array<int>) (begnRules : array<int>) (endnRules : array<int> *array<int>) nontermNumb (str : array<int>) n (matrix : array<array<array<int>>>) = 
    matrix.[n - 1] <- rulesCheckParal begtRules endtRules matrix.[n - 1] str
    printfn "%A" (toMtrx (allCYKGPU matrix nontermNumb begnRules endnRules) n n )
    toMtrx (allCYKGPU matrix nontermNumb begnRules endnRules) n n 

let CYKallParallelGPGPU (rules : array<string*string>) str start = 
    if str = ""
    then true
    else 
        let (term, nonterm, begnRules, endnRules, begtRules, endtRules) = formRules rules
        let strArr = formStr term str
        let n = str.Length 
        let nontermNumb = nonterm.Length
        let matrix =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate nonterm.Length|]|]
        let matrCYK = allCYKParallelGPGPU begtRules endtRules begnRules endnRules nontermNumb strArr n matrix
        conclCYK matrCYK nonterm start

let (rl1, start1) = ([|("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")|], "S")
let res = CYKallParallelGPGPU rl1 "baaba" start1
for i = 0 to 20 do
    printfn "%b" (CYKallParallelGPGPU rl1 "baaba" start1)

System.Console.ReadKey(true) |> ignore