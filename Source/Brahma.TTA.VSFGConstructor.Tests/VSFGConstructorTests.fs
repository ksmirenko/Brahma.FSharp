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
let PlusTest() =  
    let t = VSFGConstructor("
    let main (x:int) (y : int) (z: int) :int = x + y + z + z + y + y + z + x + y
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput1"
    rebuildPlus a
    Visualization.VSFGtoPNG a "BigXYZInputTest"

    let t = VSFGConstructor("
    let main (x:int) (y : int) (z: int) :int = (((x + y) + (z + z)) + ((y + y) + (z + x))) + y
    ")
    let a = t.getVSFG
    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = x + y  + x + y
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput2"

    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = (x + y)  + (x + y)
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput2"

    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = (x + y)  + x + y
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput2"


    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = (x + y)  + (x + y)  + x + y
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput2"


    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = (x + y)  + (x + y)  + (x + y)
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput2"



    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = (x + y)  + (x + y)  + (x + y) + (x+y)
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput2"

    
    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = ((x + y)  + (x + y))  + ((x + y) + (x+y))
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "BigXYZInput2"

    ()

[<Test>]
let Plus1Test() =  
    let t = VSFGConstructor("
    let main (x:int) (y : int) (z: int) :int =  x + y + z + y + x + z
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "3VarIn"
    let t = VSFGConstructor("
    let main (x:int) (y : int) (z: int) :int =  (x + y) + (z + y) + (x + z)
    ")

    let a = t.getVSFG
    Visualization.VSFGtoPNG a "3VarOut" 
    ()

[<Test>]
let Plus4Test() =  
    let t = VSFGConstructor("
    let main (x:int) (y : int) (z: int) :int = x + y + z + y + x + z + x + y
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "plus4"
    ()

[<Test>]
let Plus5Test() =  
    let t = VSFGConstructor("
    let main (x:int) (y : int) (z: int) :int = ((x + y) + (z + y)) + ((x + z) + (x + y))
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "Plus5"
    ()

[<Test>]
let Plus2Test() =  
    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = ((x + y) + (x + y)) + ((x + y) + (x + y))
    ")
    let a = t.getVSFG
    Visualization.VSFGtoPNG a "Plus2TestOut"

    let t = VSFGConstructor("
    let main (x:int) (y : int) :int = x + y + x + y + x + y + x + y
    ")
    let a = t.getVSFG
    //Visualization.VSFGtoPNG a "Plus2TestInp"

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