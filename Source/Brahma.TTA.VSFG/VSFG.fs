namespace Brahma.TTA.VSFG

open Brahma.TTA.VirtualTTA

type INode = interface end

(**
 * NodeStatus = Unused, if we didn't pass input edges
 *              Preparing, if we passed at least one input edge
 *              Ready, if we passed all input edges
 *              Used, if we passed all output edges
 *)
type NodeStatus = Unused | Preparing | Ready | Used

type Port () = 
    interface INode
    member val Node = Unchecked.defaultof<Node> with get, set

and InPort (inputs : ResizeArray<OutPort>) =
    inherit Port ()

    (**
     * TODO: Can be there more than 1 prevNodes?
     * Is it necessary to store (Node array) instead of (Node)?
     *)
    //do if (inputs.Count > 1) then printfn "Error. Can't in more than 1 port"

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

and Node (inPorts : ResizeArray<InPort>, outPorts : ResizeArray<OutPort>, opType : OperationType) = 
    
    interface INode

    member val inPortsCount = inPorts.Count with get, set
    member val outPortsCount = outPorts.Count with get, set

    member val Status = Unused with get, set
    member val ResultAddr = (-1<ln>, -1<col>) with get, set
    member val OpType = opType with get, set
    member val IsVisited : bool array = Array.init 1 (fun x -> false) with get, set


    member this.SetInPortsCount(count : int) = 
        this.inPortsCount <- count
    member this.SetOutPortsCount(count : int) =
        this.outPortsCount <- count

    member this.DecInPorts() = 
        this.inPortsCount <- this.inPortsCount - 1
        if this.inPortsCount = inPorts.Count - 1
        then this.Status <- Preparing

        if this.inPortsCount = 0
        then this.Status <- Ready

    member this.DecOutPorts() = 
        this.outPortsCount <- this.outPortsCount - 1

        if this.outPortsCount = 0
        then this.Status <- Used

    member this.InPorts = 
        for port in inPorts do
            port.Node <- this
        inPorts
    member this.OutPorts = 
        for port in outPorts do
            port.Node <- this
        outPorts

    member this.AddNewInPort () =
        let port = new InPort()
        port.Node <- this
        this.InPorts.Add(port)

    member this.AddNewOutPort () =
        let port = new OutPort()
        port.Node <- this
        this.OutPorts.Add(port)

    member this.GetNextNotVisitedNodes() = 
        let (allNodes : ResizeArray<Node>) = this.GetNextNodes()
        let notVisitedNodes = new ResizeArray<Node>()

        for i in [0..allNodes.Count-1] do
            if this.IsVisited.[i] = false
            then notVisitedNodes.Add(allNodes.[i])

        notVisitedNodes

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

    member this.SetNodeAsVisited(node : Node) = 
        let outNodes = this.GetNextNodes()

        for i in [0..outNodes.Count-1] do
            if outNodes.[i] = node
            then this.IsVisited.[i] <- true 

    member this.IsTriggerPort(port : int<port>) =
        port = (this.InPorts.Count - 1) * 1<port>

    (**
     * Returns (x, y), where
     * x -- outPorts from this node
     * y -- inPort from node
     *)
    member this.GetPorts(node : Node) = 
        let resInd = ref (-1<port>, -1<port>)
        
        for i in [0..outPorts.Count-1] do
            let curOutPort = outPorts.[i]
            let nextNodes = curOutPort.NextNodes
            
            (**
             * let index = 10
             * let measuredIndex = index * 1<port> -- multiply by one it just convert number to measured number
             *)
            for targetNode in nextNodes do
                if targetNode = node then
                    let prevNodes = targetNode.GetPrevNodes()
                    for j in [0..prevNodes.Count-1] do
                        if this = prevNodes.[j] then
                            resInd := ((i + inPorts.Count) * 1<port>, j * 1<port>)
        !resInd

    new (inPortsCount, outPortsCount, opType) =
        Node (new ResizeArray<_>(Array.init inPortsCount (fun _ -> new InPort())),
              new ResizeArray<_>(Array.init outPortsCount (fun _ -> new OutPort())),
              opType)
    new (opType) =
        Node (new ResizeArray<_>(), new ResizeArray<_>(), opType)

type InitialNode() =
    inherit Node (1, 1, REGISTER_TYPE)
    (* TODO: Add this.Status <- Ready *)

type TerminalNode() =
    inherit Node (1, 1, REGISTER_TYPE)

type ConstNode = 
    inherit Node
    val Value: int
    new(value: int) = 
        {inherit Node(0,1, REGISTER_TYPE); Value = value}
    
type UnaryNode (opType : OperationType) =
    inherit Node (1, 1, opType)

type BinaryNode (opType : OperationType) =
    inherit Node (2, 1, opType)

type ThreeOpNode (opType : OperationType) =
    inherit Node (3, 1, opType)

type AddNode () =
    inherit BinaryNode (ADD_TYPE)

type SubNode () =
    inherit BinaryNode (SUB_TYPE)

type DivNode () =
    inherit BinaryNode (DIV_TYPE)

type EqNode () =
    inherit BinaryNode (EQ_TYPE)

type GtNode () =
    inherit BinaryNode (GT_TYPE)

type LtNode () =
    inherit BinaryNode (LT_TYPE)

type GeqNode () =
    inherit BinaryNode (GEG_TYPE)

type LeqNode () =
    inherit BinaryNode (LEQ_TYPE)

type MultiplexorNode () = 
    inherit ThreeOpNode (MULTIPLEXOR_TYPE)


type VSFG (initialNodes : Node array, terminalNodes : Node array, constNodes : ConstNode array) =
    member this.InitialNodes = initialNodes
    member this.TerminalNodes = terminalNodes
    member this.ConstNodes = constNodes

    static member AddEdge (outPort : OutPort) (inPort : InPort) =
        outPort.AddTarget inPort
        inPort.AddInputs outPort

    static member AddEdgeByInd (outNode : Node) (outPortInd : int) (inNode : Node) (inPortInd : int) =
        VSFG.AddEdge outNode.OutPorts.[outPortInd] inNode.InPorts.[inPortInd]
        outNode.IsVisited <- Array.init ((outNode.GetNextNodes()).Count) (fun x -> false)
        outNode.SetOutPortsCount(outNode.IsVisited.Length)

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
        { inherit Node (inPorts, outPorts, REGISTER_TYPE); Vsfg = vsfg}
