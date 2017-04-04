namespace Brahma.FSharp.OpenCL.TypeProvider.Test

#if INTERACTIVE
#r "../../packages/NUnit/lib/nunit.framework.dll"
#r "../../bin/Brahma.FSharp.OpenCL.TypeProvider/Brahma.FSharp.OpenCL.TypeProvider.dll"
#endif

open NUnit.Framework
open Brahma.FSharp.OpenCL.TypeProvider.Provided

[<TestFixture>]
type TypeProviderTests() =
    let [<Literal>] sourcesPath = __SOURCE_DIRECTORY__ + "/OpenCLSources/"
    let [<Literal>] simplePath = sourcesPath + "simple.cl"
    let [<Literal>] matvecPath = sourcesPath + "matvec.cl"
    let [<Literal>] matmatPath = sourcesPath + "matmat.cl"

    let sign = dict[
                    "void", "Microsoft.FSharp.Core.Unit";
                    "char", "System.SByte";
                    "int", "System.Int32";
                    "uint", "System.UInt32";
                    "float", "System.Single"
                    ]

    let checkKernelSignature _fun _params =
        let invokeMethod = _fun.GetType().GetMethod("Invoke")
        let actualParams = invokeMethod.GetParameters().[0].ParameterType.GenericTypeArguments
                           |> Array.map (fun x -> x.FullName)
        Assert.AreEqual(sign.Item("void"), invokeMethod.ReturnType.FullName)
        Assert.AreEqual(_params, actualParams)

    [<Test>]
    member this.``Simple single kernel definition without body``() =
        let foo = KernelProvider<simplePath>.foo
        checkKernelSignature foo [| sign.Item("int"); sign.Item("char") + "[]" |]

    [<Test>]
    member this.``TP: matrix * vector, treating pointers as arrays``() =
        let matvec = KernelProvider<matvecPath, TreatPointersAsArrays=true>.matvec
        let floatArray = sign.Item("float") + "[]"
        let _params = [|
                        floatArray;
                        floatArray;
                        sign.Item("uint");
                        floatArray
                      |]
        checkKernelSignature matvec _params

    [<Test>]
    member this.``TP: matrix * matrix, treating pointers as arrays``() =
        let matmat = KernelProvider<matmatPath, TreatPointersAsArrays=true>.myGEMM1
        let _int = sign.Item("int")
        let floatArray = sign.Item("float") + "[]"
        let _params = [|
                        _int;
                        _int;
                        _int;
                        floatArray;
                        floatArray;
                        floatArray
                      |]
        checkKernelSignature matmat _params
