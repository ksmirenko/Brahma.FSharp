module Brahma.TTA.VSFG

type INode = interface end


type Port () = 
    interface INode
    member val Node = Unchecked.defaultof<Node> with get, set

and InPort (inputs : ResizeArray<OutPort>) =
    inherit Port ()
    let prevNodes = new ResizeArray<_>()
    member this.PrevNodes = prevNodes

    member this.Inputs = inputs

    new () = InPort (new ResizeArray<_>())
    new (x : OutPort) = InPort (new ResizeArray<_>([|x|]))

and OutPort (targets : ResizeArray<InPort>) =
    inherit Port ()
    let nextNodes = new ResizeArray<_>()
    member this.NextNodes = nextNodes

    member this.Targets = targets

    new () = OutPort (new ResizeArray<_>())
    new (x : InPort) = OutPort (new ResizeArray<_>([|x|]))

and Node (inPorts : ResizeArray<InPort>, outPorts : ResizeArray<OutPort>, operation) = 
    interface INode
    member this.InPorts = inPorts
    member this.OutPorts = outPorts
    member this.Operation = operation
    member this.NextNodes = 
        let res = new ResizeArray<_>()
        for port in this.OutPorts do
            res.AddRange (port.NextNodes)
        res
    member this.PrevNodes = 
        let res = new ResizeArray<_>()
        for port in this.InPorts do
            res.AddRange (port.PrevNodes)
        res
    static member AddEdge (outPort : OutPort) (inPort : InPort) =
        outPort.Targets.Add inPort
        inPort.PrevNodes.Add outPort.Node
        outPort.NextNodes.Add inPort.Node
    static member AddEdgeByInd (outNode : Node) (outPortInd : int) (inNode : Node) (inPortInd : int) =
        Node.AddEdge outNode.OutPorts.[outPortInd] inNode.InPorts.[inPortInd]
    new (operation) =
        Node (new ResizeArray<_>(), new ResizeArray<_>(), operation)

type InitialNode(outPort : OutPort) as this =
    inherit Node (new ResizeArray<_>(), new ResizeArray<_>([|outPort|]), null)
    let _ = outPort.Node <- this
    new () = InitialNode (new OutPort())

type TerminalNode(inPort : InPort) as this =
    inherit Node (new ResizeArray<_>([|inPort|]), new ResizeArray<_>(), null)
    let _ = inPort.Node <- this
    new () = TerminalNode (new InPort())

type UnaryNode (inPort : InPort, outPort : OutPort, operation) as this =
    inherit Node (new ResizeArray<_>([|inPort|]), new ResizeArray<_>([|outPort|]), operation)
    let _ = 
        inPort.Node <- this
        outPort.Node <- this
    new (operation) = UnaryNode (new InPort(), new OutPort(), operation)

type NegativeNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (~-))
    new () = NegativeNode (new InPort(), new OutPort())

type IncNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (fun x -> x + 1))
    new () = IncNode (new InPort(), new OutPort())

type DecNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (fun x -> x - 1))
    new () = DecNode (new InPort(), new OutPort())


type BinaryNode (left : InPort, right : InPort, outPort : OutPort, operation) as this =
    inherit Node (new ResizeArray<_>([|left; right|]), new ResizeArray<_>([|outPort|]), operation)
    let _ = 
        left.Node <- this
        right.Node <- this
        outPort.Node <- this
    new (operation) = BinaryNode (new InPort(), new InPort(), new OutPort(), operation)

type AddNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x + y))
    new () = AddNode (new InPort(), new InPort(), new OutPort())

type SubNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x - y))
    new () = SubNode (new InPort(), new InPort(), new OutPort())

type MulNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x * y))
    new () = MulNode (new InPort(), new InPort(), new OutPort())

type DivNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x / y))
    new () = DivNode (new InPort(), new InPort(), new OutPort())

type EqNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x = y))
    new () = EqNode (new InPort(), new InPort(), new OutPort())

type GtNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x > y))
    new () = GtNode (new InPort(), new InPort(), new OutPort())

type LtNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x < y))
    new () = LtNode (new InPort(), new InPort(), new OutPort())

type GeqNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x >= y))
    new () = GeqNode (new InPort(), new InPort(), new OutPort())

type LeqNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x <= y))
    new () = LeqNode (new InPort(), new InPort(), new OutPort())


type ThreeOpNode (op1 : InPort, op2 : InPort, op3 : InPort, outPort : OutPort, operation) as this =
    inherit Node (new ResizeArray<_>([|op1; op2; op3|]), new ResizeArray<_>([|outPort|]), operation)
    let _ =
        op1.Node <- this
        op2.Node <- this
        op3.Node <- this
        outPort.Node <- this
    new (operation) = ThreeOpNode (new InPort(), new InPort(), new InPort(), new OutPort(), operation)
        
type MultiplexorNode (predicate : InPort, _true : InPort, _false : InPort, outPort : OutPort) = 
    inherit ThreeOpNode (predicate, _true, _false, outPort, box (fun p t f -> if p then t else f))
    new () = MultiplexorNode (new InPort(), new InPort(), new InPort(), new OutPort())
