namespace Brahma.TTA.VSFG

type INode = interface end


type Port () = 
    interface INode

type InPort (inputs : ResizeArray<OutPort>) =
    inherit Port ()
    let prevNodes = new ResizeArray<_>()

    member this.Inputs = inputs
    member val Node = Unchecked.defaultof<Node> with get, set

    member this.PrevNodes = prevNodes

    member this.ConnectWith p = 
        this.Inputs.Add p
        prevNodes.Add p.Node

    new () = InPort (new ResizeArray<_>())
    new (x : OutPort) = InPort (new ResizeArray<_>([|x|]))

and OutPort (targets : ResizeArray<InPort>) =
    inherit Port ()
    let nextNodes = new ResizeArray<_>()

    member this.Targets = targets
    member val Node = Unchecked.defaultof<Node> with get, set

    member this.NextNodes = nextNodes

    member this.ConnectWith p = 
        this.Targets.Add p
        nextNodes.Add p.Node

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


type InitialNode(outPort : OutPort) as this =
    inherit Node (new ResizeArray<_>(), new ResizeArray<_>([|outPort|]), null)
    let _ = outPort.Node <- this

type TerminalNode(inPort : InPort) as this =
    inherit Node (new ResizeArray<_>([|inPort|]), new ResizeArray<_>(), null)
    let _ = inPort.Node <- this

type UnaryNode (inPort : InPort, outPort : OutPort, operation) as this =
    inherit Node (new ResizeArray<_>([|inPort|]), new ResizeArray<_>([|outPort|]), operation)
    let _ = 
        inPort.Node <- this
        outPort.Node <- this

type NegativeNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (~-))

type IncNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (fun x -> x + 1))

type DecNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (fun x -> x - 1))


type BinaryNode (left : InPort, right : InPort, outPort : OutPort, operation) as this =
    inherit Node (new ResizeArray<_>([|left; right|]), new ResizeArray<_>([|outPort|]), operation)
    let _ = 
        left.Node <- this
        right.Node <- this
        outPort.Node <- this

type AddNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x + y))

type SubNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x - y))

type MulNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x * y))

type DivNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x / y))

type EqNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x = y))

type GtNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x > y))

type LtNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x < y))

type GeqNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x >= y))

type LeqNode (left : InPort, right : InPort, outPort : OutPort) =
    inherit BinaryNode (left, right, outPort, box (fun x y -> x <= y))

type ThreeOpNode (op1 : InPort, op2 : InPort, op3 : InPort, outPort : OutPort, operation) as this =
    inherit Node (new ResizeArray<_>([|op1; op2; op3|]), new ResizeArray<_>([|outPort|]), operation)
    let _ =
        op1.Node <- this
        op2.Node <- this
        op3.Node <- this
        outPort.Node <- this
        
type MultiplexorNode (predicate : InPort, _true : InPort, _false : InPort, outPort : OutPort) = 
    inherit ThreeOpNode (predicate, _true, _false, outPort, box (fun p t f -> if p then t else f))
