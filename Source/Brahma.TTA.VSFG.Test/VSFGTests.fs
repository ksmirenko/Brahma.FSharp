module Brahma.TTA.VSFG.Tests

open Brahma.TTA.VSFG
open NUnit.Framework

let checkNeighbours (node : Node) prevCount nextCount =
    let prevNodes = node.GetPrevNodes()
    let nextNodes = node.GetNextNodes()

    Assert.NotNull(prevNodes)
    Assert.AreEqual(prevCount, prevNodes.Count)

    Assert.NotNull(nextNodes)
    Assert.AreEqual(nextCount, nextNodes.Count)

[<Test>]
let SmallGraph () =
    let x = new InitialNode()
    let y = new InitialNode()

    let terminal = new TerminalNode()

    let plus = new AddNode()
    let less = new LtNode()
    let inc = new IncNode()
    let dec = new DecNode()
    let f = new BinaryNode(null)
    let multiplexor = new MultiplexorNode()

    x.AddNewInPort()
    y.AddNewInPort()

    VSFG.AddVerticesAndEdges 
        [|
            x :> Node, 0, plus :> Node, 0;
            y :> Node, 0, plus :> Node, 1;
            x :> Node, 0, less :> Node, 0;
            y :> Node, 0, less :> Node, 1;
            x :> Node, 0, inc :> Node, 0;
            y :> Node, 0, dec :> Node, 0;

            inc :> Node, 0, f :> Node, 0;
            dec :> Node, 0, f :> Node, 1;

            inc :> Node, 0, x :> Node, 0;
            dec :> Node, 0, y :> Node, 0;

            less :> Node, 0, multiplexor :> Node, 0;
            f :> Node, 0, multiplexor :> Node, 1;
            plus :> Node, 0, multiplexor :> Node, 2;

            multiplexor :> Node, 0, terminal :> Node, 0;
        |]

    let vsfg = new VSFG ([|x; y|], [|terminal|])

    checkNeighbours x 1 3
    checkNeighbours y 1 3
    checkNeighbours terminal 1 0
    checkNeighbours plus 2 1
    checkNeighbours less 2 1
    checkNeighbours inc 1 2
    checkNeighbours dec 1 2
    checkNeighbours f 2 1
    checkNeighbours multiplexor 3 1



