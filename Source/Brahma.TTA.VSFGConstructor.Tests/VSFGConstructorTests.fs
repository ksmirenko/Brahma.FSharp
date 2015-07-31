module Brahma.TTA.VSFG.Tests

open Brahma.TTA.VSFG
open Brahma.TTA.VSFGConstructorHelper
open Brahma.TTA.VSFGConstructor
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

    f (g x) x
    ")

    t.print (fun x -> printf "visit %A" x)

    let vsfg = t.getVSFG

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
        
        
    t.print (fun x -> printfn "visit %A" x)
    let vsfg = t.getVSFG
    ()
    
[<Test>]
let Recursion() = 
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
    ()