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

module Simplest

open OpenCL.Net
open Brahma.FSharp.OpenCL.Core
open Brahma.FSharp.OpenCL.Extensions
open Brahma.FSharp.OpenCL.TypeProvider.Provided
open Brahma.Helpers
open Brahma.OpenCL

let [<Literal>] clSourcePath = __SOURCE_DIRECTORY__ + "../../../Tests/Brahma.FSharp.OpenCL.TypeProvider.Tests/OpenCLSources/simplest.cl"
type ProvidedType = KernelProvider<clSourcePath, TreatPointersAsArrays=true>

let length = 8
let gpuArr = Array.init length (fun _ -> 0)

let Main platformName (arr:array<_>) =
    let localWorkSize = 2
    let deviceType = DeviceType.Default

    let additionalClSource = System.IO.File.ReadAllText(clSourcePath)
    let simplest = fun a v ->
        try ProvidedType.simplest(a, v)
        with
        | ex -> failwith ex.Message

    let computeProvider =
        try ComputeProvider.Create(platformName, deviceType)
        with
        | ex -> failwith ex.Message
    let mutable commandQueue = new CommandQueue(computeProvider, computeProvider.Devices |> Seq.head)

    let a = arr
    let v = 42
    let command =
        <@
            fun (r:_1D) (a:array<_>) (v:int) ->
                simplest a v
        @>

    printfn "Using OpenCL and platform/device: %A ..." computeProvider
    let code = ref ""
    let kernel, kernelPrepare, kernelRun = computeProvider.Compile(command, _outCode = code, additionalSource = additionalClSource)
    let d =(new _1D(length, localWorkSize))
    kernelPrepare d a v

    let _ = commandQueue.Add(kernelRun()).Finish()

    let _ = commandQueue.Add(a.ToHost computeProvider).Finish()
    printfn "Result: %A" a

    commandQueue.Dispose()
    computeProvider.Dispose()
    computeProvider.CloseAllBuffers()

Main "NVIDIA*" gpuArr |> ignore
