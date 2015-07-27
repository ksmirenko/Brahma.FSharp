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
let WithoutRec() = 
    
    let t = VSFGConstructor("

   let main (x:int) :int =
    let g x y = x + y 
    let f (x:int) (y:int) :int =
        let e x y = x + y
        if x > y then
           g (x) (x+y)
        else 
            x - y
    f 2 x
    "
        )
    let vsfg = t.getVSFG (t.Helper.getFSharpExpr 0)
    Assert.AreEqual (vsfg.InitialNodes.Length, 1)
    
    checkNeighbours vsfg.TerminalNodes.[0] 1 0

