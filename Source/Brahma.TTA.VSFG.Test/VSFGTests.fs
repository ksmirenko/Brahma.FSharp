module Brahma.TTA.VSFG.Tests

open Brahma.TTA.VSFG
open NUnit.Framework

let calculate (node : UnaryNode) = 
    (unbox node.Operation) node.InPorts.[0]

[<Test>]
let SmallGraph () =
    let x = new InitialNode()

    let y = new InitialNode()

    let plus = new AddNode()

    Node.AddEdgeByInd x 0 plus 0
    Node.AddEdgeByInd y 0 plus 1

    let less = new LtNode()

    Node.AddEdgeByInd x 0 less 0
    Node.AddEdgeByInd y 0 less 1

    let inc = new IncNode()

    Node.AddEdgeByInd x 0 inc 0

    let dec = new DecNode()

    Node.AddEdgeByInd y 0 dec 0

    x.AddNewInPort()
    Node.AddEdgeByInd inc 0 x 0

    y.AddNewInPort()
    Node.AddEdgeByInd dec 0 y 0

    let f = new BinaryNode(null)


    let multiplexor = new MultiplexorNode()

    Node.AddEdgeByInd less 0 multiplexor 0
    Node.AddEdgeByInd f 0 multiplexor 1
    Node.AddEdgeByInd plus 0 multiplexor 2
                      
    let terminal = new TerminalNode()

    Node.AddEdgeByInd multiplexor 0 terminal 0

    Assert.NotNull(x.GetPrevNodes())
    Assert.AreEqual(1, x.GetPrevNodes().Count)

    Assert.NotNull(y.GetPrevNodes)
    Assert.AreEqual(1, y.GetPrevNodes().Count)
    


