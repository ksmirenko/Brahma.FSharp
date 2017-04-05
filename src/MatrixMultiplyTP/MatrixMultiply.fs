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

let [<Literal>] openClPath = __SOURCE_DIRECTORY__ + "../../Tests/Brahma.FSharp.OpenCL.TypeProvider.Test/OpenClSources/matmat.cl"
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
    0

Main "NVIDIA*" (makeMatrix size size) (makeMatrix size size) |> ignore
