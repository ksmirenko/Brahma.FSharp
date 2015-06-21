module Brahma.TTA.VSFG.Tests

open Brahma.TTA.VSFG
open NUnit.Framework

let calculate (node : UnaryNode) = 
    (unbox node.Operation) node.InPorts.[0]

[<Test>]
let SmallGraph () =
    let xOut = new OutPort()
    let x = new InitialNode(xOut)

    let yOut = new OutPort()
    let y = new InitialNode(yOut)

    let plusLeft = new InPort(xOut)
    let plusRight = new InPort(yOut)
    let plusOut = new OutPort()
    let plus = new AddNode(plusLeft, plusRight, plusOut)

    Node.AddEdge xOut plusRight
    Node.AddEdge yOut plusLeft

    let lessLeft = new InPort(xOut)
    let lessRight = new InPort(yOut)
    let lessOut = new OutPort()
    let less = new LtNode(lessLeft, lessRight, lessOut)

    Node.AddEdge xOut lessRight
    Node.AddEdge yOut lessLeft

    let incIn = new InPort(xOut)
    let incOut = new OutPort()
    let inc = new IncNode(incIn, incOut)

    Node.AddEdge xOut incIn

    let decIn = new InPort(yOut)
    let decOut = new OutPort()
    let dec = new DecNode(decIn, decOut)

    Node.AddEdge yOut decIn

    let xIn = new InPort(incOut)
    x.InPorts.Add(xIn)
    
    Node.AddEdge incOut xIn

    let yIn = new InPort(decOut)
    y.InPorts.Add(yIn)

    Node.AddEdge decOut yIn

    let fOut = new OutPort()
    let f = new BinaryNode(xIn, yIn, fOut, null)


    let multiplexorOut = new OutPort()
    let predicate = new InPort(lessOut)
    let _true = new InPort(fOut)
    let _false = new InPort(plusOut)
    let multiplexor = new MultiplexorNode(predicate, _true, _false, multiplexorOut)

    Node.AddEdge lessOut predicate
    Node.AddEdge fOut _true
    Node.AddEdge plusOut _false

    let terminalIn = new InPort(multiplexorOut)
    let terminal = new TerminalNode(terminalIn)

    Node.AddEdge multiplexorOut terminalIn

    Assert.NotNull(x.PrevNodes)
    Assert.AreEqual(1, x.PrevNodes.Count)

    Assert.NotNull(y.PrevNodes)
    Assert.AreEqual(1, y.PrevNodes.Count)
    


