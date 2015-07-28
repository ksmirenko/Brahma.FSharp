module Brahma.TTA.VSFGCompiler.Tests

open Brahma.TTA.VSFG
open Brahma.TTA.VirtualTTA
open Brahma.TTA.VSFGCompiler
open Brahma.TTA.VSFGConstructor
open NUnit.Framework

[<Test>]
let VSFGCompilerWithoutMUXandFunctions () = 
    let x = new InitialNode()
    let y = new InitialNode()
    
    x.ResultAddr <- (1<ln>, 0<col>)
    y.ResultAddr <- (1<ln>, 1<col>)
    x.Status <- Ready (* Can we do it automatically? *)

    let terminal = new TerminalNode()

    terminal.ResultAddr <- (1<ln>, 2<col>)

    let plus1 = new AddNode()
    let plus2 = new AddNode()
    let plus3 = new AddNode()
    let plus4 = new AddNode()
    let plus5 = new AddNode()
    let plus6 = new AddNode() 

    let vsfg = new VSFG ([|x; y|], [|terminal|], [||])

    VSFG.AddVerticesAndEdges 
        [|
            x :> Node, 0, plus1 :> Node, 0;
            y :> Node, 0, plus1 :> Node, 1;
            x :> Node, 0, plus2 :> Node, 0;
            y :> Node, 0, plus2 :> Node, 1;
            x :> Node, 0, plus3 :> Node, 0;
            y :> Node, 0, plus3 :> Node, 1;

            plus1 :> Node, 0, plus4 :> Node, 0;

            plus2 :> Node, 0, plus4 :> Node, 1
            plus2 :> Node, 0, plus5 :> Node, 0;

            plus3 :> Node, 0, plus5 :> Node, 1;

            plus4 :> Node, 0, plus6 :> Node, 0;
            plus5 :> Node, 0, plus6 :> Node, 1;

            plus6 :> Node, 0, terminal :> Node, 0;
        |]

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let TTA = new TTA([| (FU1, 5); (FU2, 4) |], 1)

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    ()

    (*
    let file = new System.IO.StreamWriter(@"C:\Users\User\Documents\Workspace\test.txt")

    code.ForEach( 
        fun x -> 
        ( 
            //printf "("
            Array.iter( fun y -> file.Write(Asm.toString(y, TTA)); file.Write("; ")) x
            file.WriteLine()
            //printfn ")"
        ) 
    )

    file.Close()
    *)

[<Test>]
let SimpleCompilingFromFSharpToAsm() = 
        
    let t = VSFGConstructor("

    let main (x:int) (y:int) :int = x + y
    "
        )
    let vsfg = t.getVSFG (t.Helper.getFSharpExpr 0)

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 2<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let TTA = new TTA([| (FU1, 5); (FU2, 4) |], 1)

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    ()

    (*
    let file = new System.IO.StreamWriter(@"C:\Users\User\Documents\Workspace\test.txt")

    code.ForEach( 
        fun x -> 
        ( 
            //printf "("
            Array.iter( fun y -> file.Write(Asm.toString(y, TTA)); file.Write("; ")) x
            file.WriteLine()
            //printfn ")"
        ) 
    )

    file.Close()
    *)

[<Test>]
let SimpleCompilingFromFSharpToAsm2() = 
        
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = x + y + z
    "
        )
    let vsfg = t.getVSFG (t.Helper.getFSharpExpr 0)

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)
    inits.[2].ResultAddr <- (1<ln>, 2<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 3<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let TTA = new TTA([| (FU1, 5); (FU2, 4) |], 2)

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    ()
    (*
    let file = new System.IO.StreamWriter(@"C:\Users\User\Documents\Workspace\test.txt")

    code.ForEach( 
        fun x -> 
        ( 
            //printf "("
            Array.iter( fun y -> file.Write(Asm.toString(y, TTA)); file.Write("; ")) x
            file.WriteLine()
            //printfn ")"
        ) 
    )

    file.Close()
    *)

[<Test>]
let MoreComplexCompilingFromFSharpToAsm() =     
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z
    "
        )
    let vsfg = t.getVSFG (t.Helper.getFSharpExpr 0)

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

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    let file = new System.IO.StreamWriter(@"C:\Users\User\Documents\Workspace\test.txt")

    code.ForEach( 
        fun x -> 
        ( 
            //printf "("
            Array.iter( fun y -> file.Write(Asm.toString(y, TTA)); file.Write("; ")) x
            file.WriteLine()
            //printfn ")"
        ) 
    )

    file.Close()