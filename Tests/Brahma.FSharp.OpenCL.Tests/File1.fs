module File1

open NUnit.Framework
open System.IO
open System
open System.Reflection


open Brahma.Helpers
open OpenCL.Net
open Brahma.OpenCL
open Brahma.FSharp.OpenCL.Core
open System
open System.Reflection
open Microsoft.FSharp.Quotations
open Brahma.FSharp.OpenCL.Extensions


[<Struct>]
type a = 
        val mutable x: int 
        val mutable y: int     
        new (x1, y1) = {x = x1; y = y1}

let defaultInArrayLength = 4
let intInArr = [|0..defaultInArrayLength-1|]
let float32Arr = Array.init defaultInArrayLength (fun i -> float32 i)
let _1d = new _1D(defaultInArrayLength, 1)
let _2d = new _2D(defaultInArrayLength, 1)
let deviceType = DeviceType.Default
let platformName = "*"

let provider =
    try  ComputeProvider.Create(platformName, deviceType)
    with
    | ex -> failwith ex.Message
 
let checkResult command =
    let kernel,kernelPrepareF, kernelRunF = provider.Compile command    
    let commandQueue = new CommandQueue(provider, provider.Devices |> Seq.head)            
    let check (outArray:array<'a>) (expected:array<'a>) =        
        let cq = commandQueue.Add(kernelRunF()).Finish()
        let r = Array.zeroCreate expected.Length
        let cq2 = commandQueue.Add(outArray.ToHost(provider,r)).Finish()
        commandQueue.Dispose()
        Assert.AreEqual(expected, r)
        provider.CloseAllBuffers()
    kernelPrepareF,check
    
let command = 
    <@ 
        fun (range:_1D) (buf:array<int>) -> 
            let (a, b) = (1, 2)
            buf.[0] <- a
            buf.[1] <- snd (a, b)
    @>
let run,check = checkResult command
run _1d intInArr  
check intInArr [|1; 2; 2; 3|]