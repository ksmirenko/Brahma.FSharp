module Brahma.TTA.VSFGCompiler.Tests

open Brahma.TTA.VSFG
open Brahma.TTA.VirtualTTA
open Brahma.TTA.VSFGCompiler
open Brahma.TTA.VSFGConstructor
open NUnit.Framework

let printCodeInFile(tta : TTA, code : ResizeArray<AsmType array>, filePath : string) = 
    let file = new System.IO.StreamWriter(filePath)

    code.ForEach( 
        fun x -> 
        ( 
            Array.iter( fun y -> file.Write(Asm.toString(y, tta)); file.Write("; ")) x
            file.WriteLine()
        ) 
    )

    file.Close()

[<Test>]
let SimpleCompilingFromFSharpToAsm() = 
        
    let t = VSFGConstructor("

    let main (x:int) (y:int) :int = x + y
    "
        )
    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 2<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let TTA = new TTA([| (FU1, 5); (FU2, 4) |], 1)

    TTA.SetFUAsNonFree(inits.[0].ResultAddr)
    TTA.SetFUAsNonFree(inits.[1].ResultAddr)
    TTA.SetFUAsNonFree(terminals.[0].ResultAddr)

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    printCodeInFile(TTA, code, "Test1.txt")

[<Test>]
let SimpleCompilingFromFSharpToAsm2() = 
        
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = x + y + z
    "
        )
    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)
    inits.[2].ResultAddr <- (1<ln>, 2<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 3<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let TTA = new TTA([| (FU1, 5); (FU2, 4) |], 2)

    TTA.SetFUAsNonFree(inits.[0].ResultAddr)
    TTA.SetFUAsNonFree(inits.[1].ResultAddr)
    TTA.SetFUAsNonFree(inits.[2].ResultAddr)
    TTA.SetFUAsNonFree(terminals.[0].ResultAddr)

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    printCodeInFile(TTA, code, "Test2.txt")


[<Test>]
let MoreComplexCompilingFromFSharpToAsm() =     
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z
    "
        )
    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)
    inits.[2].ResultAddr <- (1<ln>, 2<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 3<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let FU3 = SUB("in1", "in2t", "out1", true)
    let FU4 = DIV("in1", "in2t", "out1", true)
    let TTA = new TTA([| (FU1, 5); (FU2, 5); (FU3, 5); (FU4, 5) |], 3)

    TTA.SetFUAsNonFree(inits.[0].ResultAddr)
    TTA.SetFUAsNonFree(inits.[1].ResultAddr)
    TTA.SetFUAsNonFree(inits.[2].ResultAddr)
    TTA.SetFUAsNonFree(terminals.[0].ResultAddr)

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    printCodeInFile(TTA, code, "Test3.txt")

[<Test>]
let SimpleIfConstructions () = 
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = if x < y
                                              then
                                                x + y
                                              else
                                                x + z
                                              
    "
        )
    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)
    inits.[2].ResultAddr <- (1<ln>, 2<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 3<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let FU3 = BOOL("0", true)
    let FU4 = LT("in1", "in2t", "out1", true)
    let FU5 = DIV("in1", "in2t", "out1", true)
    let FU6 = PC("000", true)
    let TTA = new TTA([| (FU1, 2); (FU2, 10); (FU3, 5); (FU4, 5); (FU5, 1); (FU6, 1) |], 3)
    
    TTA.SetFUAsNonFree(inits.[0].ResultAddr)
    TTA.SetFUAsNonFree(inits.[1].ResultAddr)
    TTA.SetFUAsNonFree(inits.[2].ResultAddr)
    TTA.SetFUAsNonFree(terminals.[0].ResultAddr)
    
    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    printCodeInFile(TTA, code, "Test4.txt")

[<Test>]
let MoreComplexIfConstructions () = 
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = if x < y
                                              then
                                                x
                                              else
                                                z
    "
        )
    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)
    inits.[2].ResultAddr <- (1<ln>, 2<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 3<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let FU3 = BOOL("0", true)
    let FU4 = LT("in1", "in2t", "out1", true)
    let FU5 = DIV("in1", "in2t", "out1", true)
    let FU6 = PC("000", true)
    let TTA = new TTA([| (FU1, 7); (FU2, 10); (FU3, 5); (FU4, 5); (FU5, 3); (FU6, 1) |], 3)
    
    TTA.SetFUAsNonFree(inits.[0].ResultAddr)
    TTA.SetFUAsNonFree(inits.[1].ResultAddr)
    TTA.SetFUAsNonFree(inits.[2].ResultAddr)
    TTA.SetFUAsNonFree(terminals.[0].ResultAddr)
    
    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    printCodeInFile(TTA, code, "Test5.txt")