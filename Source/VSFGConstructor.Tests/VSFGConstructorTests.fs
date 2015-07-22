module Brahma.TTA.VSFG.Tests

open Brahma.TTA.VSFG
open Brahma.TTA.VSFGConstructorHelper

open NUnit.Framework

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

   
