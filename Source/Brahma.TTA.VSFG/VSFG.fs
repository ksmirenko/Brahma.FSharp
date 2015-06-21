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

and Node (inPorts : ResizeArray<InPort>, outPorts : ResizeArray<OutPort>, operation) as this = 
    interface INode

    member this.InPorts = 
        for port in inPorts do
            port.Node <- this
        inPorts
    member this.OutPorts = 
        for port in outPorts do
            port.Node <- this
        outPorts
    member this.Operation = operation
    member this.GetNextNodes () = 
        let res = new ResizeArray<_>()
        for port in this.OutPorts do
            res.AddRange (port.NextNodes)
        res
    member this.GetPrevNodes () = 
        let res = new ResizeArray<_>()
        for port in this.InPorts do
            res.AddRange (port.PrevNodes)
        res
    member this.AddNewInPort () =
        let port = new InPort()
        port.Node <- this
        this.InPorts.Add(port)
    member this.AddNewOutPort () =
        let port = new OutPort()
        port.Node <- this
        this.OutPorts.Add(port)
        
    static member AddEdge (outPort : OutPort) (inPort : InPort) =
        outPort.Targets.Add inPort
        inPort.PrevNodes.Add outPort.Node
        outPort.NextNodes.Add inPort.Node
    static member AddEdgeByInd (outNode : Node) (outPortInd : int) (inNode : Node) (inPortInd : int) =
        Node.AddEdge outNode.OutPorts.[outPortInd] inNode.InPorts.[inPortInd]
    new (operation) =
        Node (new ResizeArray<_>(), new ResizeArray<_>(), operation)

type InitialNode() =
    inherit Node (new ResizeArray<_>(), new ResizeArray<_>([|new OutPort()|]), null)

type TerminalNode() =
    inherit Node (new ResizeArray<_>([|new InPort()|]), new ResizeArray<_>(), null)


type UnaryNode (operation) =
    inherit Node (new ResizeArray<_>([|new InPort()|]), new ResizeArray<_>([|new OutPort()|]), operation)
    new () = UnaryNode (null)

type NegativeNode () =
    inherit UnaryNode (box (~-))

type IncNode () =
    inherit UnaryNode (box (fun x -> x + 1))

type DecNode () =
    inherit UnaryNode (box (fun x -> x - 1))


type BinaryNode (operation) =
    inherit Node (new ResizeArray<_>([|new InPort(); new InPort()|]), new ResizeArray<_>([|new OutPort()|]), operation)
    new () = BinaryNode (null)

type AddNode () =
    inherit BinaryNode (box (fun x y -> x + y))

type SubNode () =
    inherit BinaryNode (box (fun x y -> x - y))

type MulNode () =
    inherit BinaryNode (box (fun x y -> x * y))

type DivNode () =
    inherit BinaryNode (box (fun x y -> x / y))

type EqNode () =
    inherit BinaryNode (box (fun x y -> x = y))

type GtNode () =
    inherit BinaryNode (box (fun x y -> x > y))

type LtNode () =
    inherit BinaryNode (box (fun x y -> x < y))

type GeqNode () =
    inherit BinaryNode (box (fun x y -> x >= y))

type LeqNode () =
    inherit BinaryNode (box (fun x y -> x <= y))


type ThreeOpNode (operation) =
    inherit Node (new ResizeArray<_>([|new InPort(); new InPort(); new InPort()|]), new ResizeArray<_>([|new OutPort()|]), operation)
    new () = ThreeOpNode (null)

type MultiplexorNode () = 
    inherit ThreeOpNode (box (fun p t f -> if p then t else f))