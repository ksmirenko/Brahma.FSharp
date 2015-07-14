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

    member this.AddInputs outPort = 
        this.Inputs.Add outPort
        this.PrevNodes.Add outPort.Node

    new () = InPort (new ResizeArray<_>())
    new (x : OutPort) = InPort (new ResizeArray<_>([|x|]))

and OutPort (targets : ResizeArray<InPort>) =
    inherit Port ()
    let nextNodes = new ResizeArray<_>()
    member this.NextNodes = nextNodes

    member this.Targets = targets

    member this.AddTarget inPort = 
        this.Targets.Add inPort
        this.NextNodes.Add inPort.Node

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

    new (inPortsCount, outPortsCount, operation) =
        Node (new ResizeArray<_>(Array.init inPortsCount (fun _ -> new InPort())),
              new ResizeArray<_>(Array.init outPortsCount (fun _ -> new OutPort())),
              operation)
    new (operation) =
        Node (new ResizeArray<_>(), new ResizeArray<_>(), operation)

type InitialNode() =
    inherit Node (0, 1, null)

type TerminalNode() =
    inherit Node (1, 0, null)

type UnaryNode (operation) =
    inherit Node (1, 1, operation)
    new () = UnaryNode (null)

type BinaryNode (operation) =
    inherit Node (2, 1, operation)
    new () = BinaryNode (null)

type ThreeOpNode (operation) =
    inherit Node (3, 1, operation)
    new () = ThreeOpNode (null)


type NegativeNode () =
    inherit UnaryNode (box (~-))

type IncNode () =
    inherit UnaryNode (box (fun x -> x + 1))

type DecNode () =
    inherit UnaryNode (box (fun x -> x - 1))


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


type MultiplexorNode () = 
    inherit ThreeOpNode (box (fun p t f -> if p then t else f))


type VSFG (initialNodes : Node array, terminalNodes : Node array) =
    member this.InitialNodes = initialNodes
    member this.TerminalNodes = terminalNodes

    static member AddEdge (outPort : OutPort) (inPort : InPort) =
        outPort.AddTarget inPort
        inPort.AddInputs outPort

    static member AddEdgeByInd (outNode : Node) (outPortInd : int) (inNode : Node) (inPortInd : int) =
        VSFG.AddEdge outNode.OutPorts.[outPortInd] inNode.InPorts.[inPortInd]

    static member AddVerticesAndEdges (toAdd : (Node * int * Node * int) array) =
        toAdd 
        |> Array.iter (fun (outNode, outInd, inNode, inInd) -> VSFG.AddEdgeByInd outNode outInd inNode inInd)

type NestedVsfgNode = 
    inherit Node
    val Vsfg : VSFG
    new (vsfg : VSFG) =
        let inPorts = 
            let ports = new ResizeArray<_> ()
            vsfg.InitialNodes |> Array.iter (fun n -> ports.AddRange (n.InPorts))
            ports
        let outPorts =
            let ports = new ResizeArray<_> ()
            vsfg.TerminalNodes |> Array.iter (fun n -> ports.AddRange (n.OutPorts))
            ports
        { inherit Node (inPorts, outPorts, null); Vsfg = vsfg}
