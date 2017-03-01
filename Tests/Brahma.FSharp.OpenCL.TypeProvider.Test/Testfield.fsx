#r @"../../src/Brahma.FSharp.OpenCL.TypeProvider/bin/Release/Brahma.FSharp.OpenCL.TypeProvider.dll"

open Brahma.FSharp.OpenCL.TypeProvider.Provided

let [<Literal>] sourcesPath = __SOURCE_DIRECTORY__ + @"\OpenCLSources\"

// Example 1: simple.cl

let [<Literal>] simpleSourcePath = sourcesPath + "simple.cl"
type SimpleProvided = KernelProvider<simpleSourcePath>
let foo = SimpleProvided.foo

let a:int = 326
let b:sbyte array = Array.create 5 <| (sbyte)42
foo(a, b)

// Example 2: matvec.cl

let [<Literal>] matvecSourcePath = sourcesPath + "matvec.cl"
type MatvecProvided = KernelProvider<matvecSourcePath>
let matvec = MatvecProvided.matvec

// stub; actually pointers to float arrays are expected here
let A = ref <| (float32)4.0
let x = ref <| (float32)(-1.44)
let y = ref <| (float32)5.0
matvec(A, x, (uint32)3, y)
