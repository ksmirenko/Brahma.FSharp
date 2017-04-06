// Copyright (c) 2017 Kirill Smirenko <k.smirenko@gmail.com>
// All rights reserved.
// 
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
// 
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
// 
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.

module MatrixMultiplyTP

open OpenCL.Net
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.TypeProvider.Provided
open Brahma.Helpers
open Brahma.OpenCL

let [<Literal>] clSourcePath = __SOURCE_DIRECTORY__ + "../../../Tests/Brahma.FSharp.OpenCL.TypeProvider.Test/OpenCLSources/matmat.cl"
let size = 10 // matrix size
let iterations = 10

let random = new System.Random()

let makeMatrix rows cols =
    Array.init (rows * cols) (fun i -> float32 (random.NextDouble()))

let outputMatrixDimensions aRows aCols bRows bCols =
    if aCols = bRows
    then aRows, bCols
    else failwith "Cannot multiply these two matrices"

let multiply (a:array<_>) aRows aCols (b:array<_>) bRows bCols (c:array<_>) =
    let cRows, cCols = outputMatrixDimensions aRows aCols bRows bCols
    for i in 0 .. cRows - 1 do
        for j in 0 .. cCols - 1 do
            let mutable buf = 0.0f
            for k in 0 .. aCols - 1 do
                 buf <- buf + a.[i * aCols + k] * b.[k * bCols + j]
            c.[i * cCols + j] <- c.[i * cCols + j] + buf


let Main platformName (m1:array<_>) (m2:array<_>) = 
    let rows = size
    let columns = size
    let localWorkSize = 2
    let deviceType = DeviceType.Default

    let additionalClSource = System.IO.File.ReadAllText(clSourcePath)
    let clMultiply =
        try KernelProvider<clSourcePath, TreatPointersAsArrays=true>.myGEMM1
        with
        | ex -> failwith ex.Message

    let computeProvider =
        try ComputeProvider.Create(platformName, deviceType)
        with
        | ex -> failwith ex.Message
    let mutable commandQueue = new CommandQueue(computeProvider, computeProvider.Devices |> Seq.head)

    let aValues = m1
    let bValues = m2
    let cParallel = Array.zeroCreate(rows * columns)

    let command =
        <@
            fun (r:_2D) (a:array<_>) (b:array<_>) (c:array<_>) ->
                clMultiply(size, size, size, a, b, c)
        @>

    printfn "Multiplying two %Ax%A matrices %A times using .NET..." rows columns iterations
    let cNormal = Array.zeroCreate (rows * columns)
    for i in 0 .. iterations - 1 do
        Timer<string>.Global.Start()
        multiply aValues rows columns bValues rows columns cNormal
        Timer<string>.Global.Lap(".NET")
    printfn "done."

    printfn "Multiplying two %Ax%A matrices %A times using OpenCL and platform/device: %A ..." rows columns iterations computeProvider
    let kernel, kernelPrepare, kernelRun = computeProvider.Compile command
    let d =(new _2D(rows, columns, localWorkSize, localWorkSize))
    kernelPrepare d aValues bValues cParallel

    for i in 0 .. iterations - 1 do
        Timer<string>.Global.Start()
        let _ = commandQueue.Add(kernelRun()).Finish()
        Timer<string>.Global.Lap("OpenCL")

    let _ = commandQueue.Add(cParallel.ToHost computeProvider).Finish()

    printfn "Verifying results..."
    let mutable isSuccess = true
    for i in 0 .. rows * columns - 1 do
        if isSuccess && System.Math.Abs(float32 (cParallel.[i] - cNormal.[i])) > 0.01f
        then
            isSuccess <- false
            printfn "Expected: %A Actual: %A Error = %A" cNormal.[i] cParallel.[i] (System.Math.Abs(cParallel.[i] - cNormal.[i]))

    printfn "done."

    Timer<string>.Global.Average(".NET") |> printfn "Avg. time, F#: %A"
    Timer<string>.Global.Average("OpenCL") |> printfn "Avg. time, OpenCL: %A"

    commandQueue.Dispose()
    computeProvider.Dispose()
    computeProvider.CloseAllBuffers()

Main "NVIDIA*" (makeMatrix size size) (makeMatrix size size) |> ignore
