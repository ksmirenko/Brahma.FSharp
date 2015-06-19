namespace Brahma.TTA.VSFG

type INode = interface end


type Port () = 
    interface INode

type InPort (inputs : ResizeArray<OutPort>) =
    inherit Port ()
    member this.Inputs = inputs
    member this.ConnectWith p = this.Inputs.Add p
    new () = InPort (new ResizeArray<_>())
    new (x : OutPort) = InPort (new ResizeArray<_>([|x|]))

and OutPort (targets : ResizeArray<InPort>) =
    inherit Port ()
    member this.Targets = targets
    member this.ConnectWith p = this.Targets.Add p
    new () = OutPort (new ResizeArray<_>())
    new (x : InPort) = OutPort (new ResizeArray<_>([|x|]))


type Node (inPorts : ResizeArray<InPort>, outPorts : ResizeArray<OutPort>, operation) = 
    interface INode
    member this.InPorts = inPorts
    member this.OutPorts = outPorts
    member this.Operation = operation


type UnaryNode (inPort : InPort, outPort : OutPort, operation) =
    inherit Node (new ResizeArray<_>([|inPort|]), new ResizeArray<_>([|outPort|]), operation)

type NegativeNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (~-))

type IncNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (fun x -> x + 1))

type DecNode (inPort : InPort, outPort : OutPort) =
    inherit UnaryNode (inPort, outPort, box (fun x -> x - 1))


type BinaryNode (left : InPort, right : InPort, outPort : OutPort, operation) =
    inherit Node (new ResizeArray<_>([|left; right|]), new ResizeArray<_>([|outPort|]), operation)

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