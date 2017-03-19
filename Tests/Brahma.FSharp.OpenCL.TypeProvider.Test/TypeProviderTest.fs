namespace Brahma.FSharp.OpenCL.TypeProvider.Test

#if INTERACTIVE
#r "../../packages/NUnit/lib/nunit.framework.dll"
#r "../../bin/Brahma.FSharp.OpenCL.TypeProvider/Brahma.FSharp.OpenCL.TypeProvider.dll"
#endif

open NUnit.Framework
open Brahma.FSharp.OpenCL.TypeProvider.Provided

[<TestFixture>]
type TypeProviderTests() =
    let sign = dict[
                    "void", "Microsoft.FSharp.Core.Unit";
                    "char", "System.SByte";
                    "int", "System.Int32";
                    "uint", "System.UInt32";
                    "float", "System.Single"
                   ]

    let checkKernelSignature _fun _params =
        let invokeMethod = _fun.GetType().GetMethods() |> Array.find (fun x -> x.Name = "Invoke")
        Assert.AreEqual(sign.Item("void"), invokeMethod.ReturnType.FullName)
        Assert.AreEqual(_params, invokeMethod.ReturnType.FullName)

    [<Test>]
    member this.``Simple single kernel definition without body``() = 
        let foo = KernelProvider<"OpenCLSources/simple.cl">.foo
        checkKernelSignature foo [| sign.Item("int"); sign.Item("char") + "[]" |]

    [<Test>]
    member this.``TP: matrix * vector, treating pointers as arrays``() =
        let matvec = KernelProvider<"OpenCLSources/matvec.cl", TreatPointersAsArrays>.matvec
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
        let matvec = KernelProvider<"OpenCLSources/matmat.cl", TreatPointersAsArrays>.matvec
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
        checkKernelSignature matvec _params
