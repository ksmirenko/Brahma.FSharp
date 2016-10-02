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
            fun (rng : _1D) (formStr : array<_>) lenR (endtR : array<_>) (begtR : array<_>) rows cols (toMtrxel : array<_>) ->
                let r = rng.GlobalID0
                for k = 0 to lenR - 1 do
                    if endtR.[k] = formStr.[r]
                    then 
                        toMtrxel.[r * cols + begtR.[k]] <- 1
        @>
    let length = str.Length 
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let d = new _1D(length, 1)
    let toMtrxel = toArr mtrxel
    kernelPrepare d str endtRules.Length endtRules begtRules rows cols toMtrxel
    let _ = commandQueue.Add(kernelRun())
    result toMtrxel rows cols provider commandQueue

let compCheckParal (mtrx : array<array<array<_>>>) nontermNumb (begnRules : array<int>) (endnRules : array<int> * array<int>) =
    let (endnRulesL, endnRulesR) = fst endnRules, snd endnRules
    let n = rows mtrx
    let l = n * n * nontermNumb
    let (provider, commandQueue) = provAndQueue()
    let command = 
        <@ 
             fun (rng : _1D) n nontermNumb (begnRules : array<int>) (endnRulesL : array<int>) enLen (endnRulesR : array<int>) (newMtrx : array<int>) (indArr : array<int>) ->
                let r = rng.GlobalID0                
                if n - indArr.[0] - 1 >= r
                then 
                    for i = 0 to nontermNumb - 1 do
                        let shift1 = (n - 1 - indArr.[1]) * n * nontermNumb + r * nontermNumb
                        if newMtrx.[shift1 + i] = 1
                        then 
                            for j = 0 to nontermNumb - 1 do
                                let shift2 = (indArr.[1] + n - indArr.[0]) * n * nontermNumb + (indArr.[1] + r + 1) * nontermNumb
                                if newMtrx.[shift2 + j] = 1
                                then 
                                    for k = 0 to enLen - 1 do
                                        if (i = endnRulesL.[k]) && (j = endnRulesR.[k])
                                        then newMtrx.[(n - 1 - indArr.[0]) * n * nontermNumb + r * nontermNumb + begnRules.[k]] <- 1
        @>

    let rng = new _1D(l, nontermNumb)
    let kernel, kernelPrepare, kernelRun = provider.Compile command
    let newMtrx = toArr (toArr mtrx)
    let indexArr = [|1; 0|]
    kernelPrepare rng n nontermNumb begnRules endnRulesL endnRulesR.Length endnRulesR newMtrx indexArr
    for a = 1 to n - 1 do
        for b = 0 to a - 1 do 
            indexArr.[0] <- a
            indexArr.[1] <- b
            commandQueue.Add(indexArr.ToGpu provider) |> ignore
            commandQueue.Add(kernelRun()) |> ignore
    result newMtrx (n * n) nontermNumb provider commandQueue

let matrixCYKParallelGPGPU (begtRules : array<int>) (endtRules : array<int>) (begnRules : array<int>) (endnRules : array<int> *array<int>) nontermNumb (str : array<int>) n (matrix : array<array<array<int>>>) = 
    matrix.[n - 1] <- rulesCheckParal begtRules endtRules matrix.[n - 1] str
    toMtrx (compCheckParal matrix nontermNumb begnRules endnRules) n n 

let CYKParallelGPGPU (rules : array<string*string>) str start = 
    if str = ""
    then true
    else 
        let (term, nonterm, begnRules, endnRules, begtRules, endtRules) = formRules rules
        let strArr = formStr term str
        let n = str.Length 
        let nontermNumb = nonterm.Length
        let matrix =  [|for i in 0..n - 1 -> [|for j in 0..n - 1 -> Array.zeroCreate nonterm.Length|]|]
        let matrCYK = matrixCYKParallelGPGPU begtRules endtRules begnRules endnRules nontermNumb strArr n matrix
        conclCYK matrCYK nonterm start