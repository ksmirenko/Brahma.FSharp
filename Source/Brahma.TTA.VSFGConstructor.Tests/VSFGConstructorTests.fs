module Brahma.TTA.VSFG.Tests

open Brahma.TTA.VSFG
open Brahma.TTA.VSFGConstructorHelper
open Brahma.TTA.VSFGConstructor
open Brahma.TTA.VSFG.Visualization
open Brahma.TTA.VSFG.Optimize

open System.Collections.Generic

open NUnit.Framework
open Microsoft.FSharp.Compiler.SourceCodeServices

let checkNeighbours (node : Node) prevCount nextCount =
    let prevNodes = node.GetPrevNodes()
    let nextNodes = node.GetNextNodes()

    Assert.NotNull(prevNodes)
    Assert.AreEqual(prevCount, prevNodes.Count)

    Assert.NotNull(nextNodes)
    Assert.AreEqual(nextCount, nextNodes.Count)

[<TestFixture>]
(*
    We learn to draw VSFG and to simplify it
*)
type PlusRebuildTester () = 
    [<Test>]
    member this.PlusTest() =  
        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int = x + y + z + z + y + y + z + x + y
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "XYZ_inp"
        rebuildPlus a
        Visualization.VSFGtoPNG a "XYZ_optimize"

        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int = (((x + y) + (z + z)) + ((y + y) + (z + x))) + y
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "XYZ_Answer"
        
    [<Test>]
    member this.OneVarTest() =  
        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int =  x + x + x + x + x + x + x + x + x + x + x + x + x
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "OneVar_Inp"
        rebuildPlus a 
        Visualization.VSFGtoPNG a "OneVar_Optimize"

        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int =  (((x + x) + (x + x)) + ((x + x) + (x + x))) + ((x + x) + (x + x)) + x
        ")

        let a = t.getVSFG
        Visualization.VSFGtoPNG a "OneVar_Ans" 
        ()

    [<Test>]
    member this.bigbigbig() =  
        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int = 
            x + y + z + y + x + z + x + y + x + z + x + y + x + z + x + y + x + z + x + y + x + z + x + y + x + z + x + y + x + z + x + y + x + z + x + y

        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "bigbigbig_Inp"
        rebuildPlus a
        Visualization.VSFGtoPNG a "bigbigbig_Optimize"

        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int = 
            ((((x + y) + (z + y)) + ((x + z) + (x + y))) + (((x + z) + (x + y)) + ((x + z) + (x + y)))) + ((((x + z) + (x + y)) + ((x + z) + (x + y))) + (((x + z) + (x + y)) + ((x + z) + (x + y)))) + ((x + z) + (x + y))

        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "bigbigbig_Ans"


        ()

    [<Test>]
    member this.Simple() =  
        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int = x + y + y + x + y + x + x + y
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "Simple_Inp"
        rebuildPlus a
        Visualization.VSFGtoPNG a "Simple_Optimize"

        let t = VSFGConstructor("
        let main (x:int) (y : int) (z: int) :int = ((x + y) + (y + x)) + ((y + x) + (x + y))
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "Simple_Ans"


        ()

    [<Test>]
    member this.double() =  
        let t = VSFGConstructor("
        let main (x:int) (y : int) :int = (x + y + x + y) * (x + y + x + y)
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "double_Inp"
        rebuildPlus a
        Visualization.VSFGtoPNG a "double_Optimize"

        let t = VSFGConstructor("
        let main (x:int) (y : int) :int = ((x + y) + (x + y)) * ((x + y) + (x + y))
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "double_ans"

        ()

    [<Test>]
    member this.RightSideBuild() =  
        let t = VSFGConstructor("
        let main (x:int) (y : int) :int = (x + (y + (x + (y + (x + (y + (x + y)))))))
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "right_Inp"
        rebuildPlus a
        Visualization.VSFGtoPNG a "right_Optimize"

        let t = VSFGConstructor("
        let main (x:int) (y : int) :int = ((x + y) + (x + y)) + ((x + y) + (x + y))
        ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "right_ans"

        ()
    [<Test>]
    member this.Dream() =  
        let t = VSFGConstructor("
        let main (x:int) (y : int) :int = x + y + x + y + (x + y) + x + y
                ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "dream_Inp"
        rebuildPlus a
        Visualization.VSFGtoPNG a "dream_Optimize"

        let t = VSFGConstructor("
        let main (x:int) (y : int) :int = ((x + y) + (x + y)) + ((x + y) + (x + y))
                ")
        let a = t.getVSFG
        Visualization.VSFGtoPNG a "dream_Ans"
        



[<Test>]
let VSFGConstructorHelper() = 
    let input = "
    open System
    let rec f x y = 
        if x < y then
            f (x+2) (y-1)
        else
           x + y
    "

    let t = new TypedTreeGetter(input) 
    Assert.AreEqual (t.CountOfDecl, 1)
    Assert.AreEqual ((0 |> t.getFSharpArgs).Length, 2)
    let input2 = "
    open System
    let rec f x y z= 
        if x < y then
            f (x+2) (y-1)
        else
           x + y
    let sum x + y = x + y
    "
    let t = new TypedTreeGetter(input2) 
    Assert.AreEqual (t.CountOfDecl, 2)
    Assert.AreEqual ((0 |> t.getFSharpArgs).Length, 3)

[<Test>]
let DeepNested() = 

    let t = VSFGConstructor("

   let main (x:int) :int =
    let rec g (x:int) : int= 
        let f (x:int) : int = 
            let e (r:int) :int = g r 
            let h :int = e 2
            
            123
        123 
   
    let rec f (x:int) (y:int) :int = 
        let g (x:int) (t:int) (y:int) = x 
        if x + 2 > y then
            f (x + 2) x
        else 
            g x y 2

    f (g 2) 3

    ")

    let vsfg = t.getVSFG

    ()


[<Test>]
let IfTest() =  
    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = 
        if x < y then 
            if x > 3 then
                x
            else y * x + 2
        else y 
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "IF"
    ()


[<Test>] 
let anotherTest() =
   let t = VSFGConstructor("
    let rec main (x:int) (y: int) = main (x + y) (y + x)
        ")
   let v = t.getVSFG
   Visualization.VSFGtoPNG v "mainRec"
   

   ()
[<Test>]
let WithoutRec() = 
    
    let t = VSFGConstructor("

   let main (x:int) :int =
    let g (x:int) (y:int) :int = 123
    let  f (x:int) (y:int) :int = 
        if x + 2 > y then
            g (x + 2) (x-y+2*x)
        else 
            g x y
    f x x
    ")
        
    let vsfg = t.getVSFG
    Visualization.VSFGtoPNG vsfg "WithoutRec"

    ()
    
[<Test>]
let Rec() = 
    let t = VSFGConstructor("

   let main (x:int) :int =
    let rec f x y = 
        if x > y then
            f (x + 2) (y)
        else x
    f x x  
    "
        )

    let vsfg = t.getVSFG 
    Visualization.VSFGtoPNG vsfg "Rec"

    ()