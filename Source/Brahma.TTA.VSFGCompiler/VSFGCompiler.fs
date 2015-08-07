namespace Brahma.TTA.VSFGCompiler

open Brahma.TTA.VirtualTTA
open Brahma.TTA.VSFG
open Brahma.TTA.VSFGConstructor

type Addr = int<ln> * int<col> * int<port>

type AsmType =
    | Mov of Addr * Addr
    | Mvc of int * Addr
    | IfTrue of Addr * Addr * Addr
    | IfTrueConst of Addr * int * Addr
    | IfFalse of Addr * Addr * Addr
    | IfFalseConst of Addr * int * Addr

module Asm = 
    let emptyAddr = (-1<ln>, -1<col>, -1<port>)
    
    let addrToString (addr : Addr, tta : TTA) = 
        match addr with
        | (-1<ln>, -1<col>, -1<port>) -> ""
        | (l, c, p) -> FunctionUnit.toString (tta.GetFU(l, c)) c p 
    
    let toString (asm : AsmType, tta : TTA) = 
        match asm with
        | Mov ((-1<ln>, -1<col>, -1<port>), (-1<ln>, -1<col>, -1<port>)) -> "..."
        | Mov(src, dst) -> (addrToString (src, tta)) + " -> " + (addrToString (dst, tta))
        | Mvc(number, dst) -> number.ToString() + " -> " + (addrToString (dst, tta))
        | IfTrue(prd, src, dst) -> "?" + (addrToString (prd, tta)) + " " + (addrToString (src, tta)) + " -> " + (addrToString (dst, tta))
        | IfTrueConst(prd, number, dst) -> "?" + (addrToString (prd, tta)) + " " + number.ToString() + " -> " + (addrToString (dst, tta))
        | IfFalse(prd, src, dst) -> "!" + (addrToString (prd, tta)) + " " + (addrToString (src, tta)) + " -> " + (addrToString (dst, tta))
        | IfFalseConst(prd, number, dst) -> "!" + (addrToString (prd, tta)) + " " + number.ToString() + " -> " + (addrToString (dst, tta))


type VSFGCompiler(vsfg : VSFG, tta : TTA) = 
    let asmCode = new ResizeArray<AsmType array>()
    let readies = new ResizeArray<Node>()
    let readyEdges = new ResizeArray<Edge>()

    member this.Vsfg = vsfg
    member this.Tta = tta

    member this.Compile() = 
        //TODO: Modify 'comand' -> 'command'
        let curGlobalComandIndex = ref 0
        let curMicroComandIndex = ref 0

        (**
         * Moves some node (if node with same type contains in unused nodes) with status = Ready to register
         *)
        let rec TryToMoveReadyNodeToRegister() = 
            let prevNodes = new ResizeArray<Node>()
            let nextUnusedNodes = new ResizeArray<Node>()
            for edge in readyEdges do
                prevNodes.Add(edge.SrcNode)
                if edge.DstNode.Status = Unused && (not (this.Tta.IsFreeFU(edge.DstNode.OpType)))
                then
                    nextUnusedNodes.Add(edge.DstNode)

            let isFound = ref false
            for nextNode in nextUnusedNodes do 
                for prevNode in prevNodes do
                   if nextNode.OpType = prevNode.OpType && not !isFound
                    then
                        isFound := true
                        ignore(SwapNodeWithRegister(prevNode))
            !isFound

        (**
         * Updates @microCommandIndex and @curGlobalCommandIndex to the next instruction
         * If at the previous stage is wasn't change microCommandIndex -> May be we haven't any resources for it, so try to move some ready register node to register 
         *)
        and CheckEndOfInstruction() =
            let isMoved = ref false
            
            if !curMicroComandIndex = 0
            then
                isMoved := TryToMoveReadyNodeToRegister()

            if !isMoved || !curMicroComandIndex <> 0
            then   
                curMicroComandIndex := 0
                curGlobalComandIndex := !curGlobalComandIndex + 1
                this.Tta.ReleaseAllBuses()
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))

        and SetEdgeAsUsed(edge : Edge) = 
            for i in [0..readyEdges.Count-1] do
                if edge = readyEdges.[i]
                then
                    readyEdges.[i].IsUsed <- true

        and GenerateCommandToVsfgType(edge : Edge) = 
            let srcNode = edge.SrcNode
            let dstNode = edge.DstNode

            let nestedVsfgNode = (dstNode :?> NestedVsfgNode)
            let inPort = nestedVsfgNode.Vsfg.InitialNodes.[int edge.InPort.Index].InPorts.[0]
            //Here we can add checking if necessary RF.i is free
            GenerateCommand(Edge(edge.OutPort, inPort))

        and GenerateCommandFromConstType(edge : Edge) = 
            let srcNode = edge.SrcNode
            let dstNode = edge.DstNode

            let (dstLn : int<ln>, dstCol : int<col>) = dstNode.ResultAddr
            let dstPort = edge.InPort.Index

            let asmCommand = Mvc((srcNode :?> ConstNode).Value, (dstLn, dstCol, dstPort))
            asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmCommand

            curMicroComandIndex := !curMicroComandIndex + 1

        and GenerateCommandFromMultiplexorType(edge : Edge) = 
            let srcNode = edge.SrcNode
            let dstNode = edge.DstNode

            let predicateEdge = Edge(srcNode.InPorts.[0].Inputs.[0], srcNode.InPorts.[0])
            let falseEdge = (Edge(srcNode.InPorts.[1].Inputs.[0], srcNode.InPorts.[1]))
            let trueEdge = (Edge(srcNode.InPorts.[2].Inputs.[0], srcNode.InPorts.[2]))

            let srcPortPredicate = 0<port>
            let srcPortFalse = falseEdge.OutPort.Index
            let srcPortTrue = trueEdge.OutPort.Index

            let dstPort = edge.InPort.Index

            let (srcLnPredicate : int<ln>, srcColPredicate : int<col>) = edge.SrcNode.ResultAddr
            let (srcLnFalse : int<ln>, srcColFalse : int<col>) = falseEdge.SrcNode.ResultAddr
            let (srcLnTrue : int<ln>, srcColTrue : int<col>) = trueEdge.SrcNode.ResultAddr

            let (dstLn : int<ln>, dstCol : int<col>) = edge.DstNode.ResultAddr

            let asmTrueCommand = ref (Mov(Asm.emptyAddr, Asm.emptyAddr))

            if (srcNode.InPorts.[2].Inputs.[0].Node.OpType = VSFG_TYPE)
            then
                asmTrueCommand := IfTrueConst((srcLnPredicate, srcColPredicate, srcPortPredicate), 0, (srcLnTrue, srcColTrue, srcPortTrue))  
            elif (srcNode.InPorts.[2].Inputs.[0].Node.OpType = CONST_TYPE)
            then
                let constNode = (srcNode.InPorts.[2].Inputs.[0].Node :?> ConstNode)
                asmTrueCommand := IfTrueConst((srcLnPredicate, srcColPredicate, srcPortPredicate), constNode.Value, (dstLn, dstCol, dstPort))
            else 
                asmTrueCommand := IfTrue((srcLnPredicate, srcColPredicate, srcPortPredicate), (srcLnTrue, srcColTrue, srcPortTrue), (dstLn, dstCol, dstPort))
                    
            asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- !asmTrueCommand

            curMicroComandIndex := !curMicroComandIndex + 1
            this.Tta.TakeABus()

            if (srcNode.InPorts.[2].Inputs.[0].Node.OpType = VSFG_TYPE)
            then    
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))     
                curMicroComandIndex := 0
                curGlobalComandIndex := !curGlobalComandIndex + 4
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))     
                this.Tta.ReleaseAllBuses()

            if (not (this.Tta.IsFreeBus()))
            then
                CheckEndOfInstruction()


            let asmFalseCommand = ref (Mov(Asm.emptyAddr, Asm.emptyAddr))

            if (srcNode.InPorts.[1].Inputs.[0].Node.OpType = VSFG_TYPE)
            then
                asmFalseCommand := IfFalseConst((srcLnPredicate, srcColPredicate, srcPortPredicate), 0, (srcLnFalse, srcColFalse, srcPortFalse))  
            elif (srcNode.InPorts.[1].Inputs.[0].Node.OpType = CONST_TYPE)
            then
                let constNode = (srcNode.InPorts.[1].Inputs.[0].Node :?> ConstNode)
                asmFalseCommand := IfFalseConst((srcLnPredicate, srcColPredicate, srcPortPredicate), constNode.Value, (dstLn, dstCol, dstPort))
            else 
                asmFalseCommand := IfFalse((srcLnPredicate, srcColPredicate, srcPortPredicate), (srcLnFalse, srcColFalse, srcPortFalse), (dstLn, dstCol, dstPort))

            asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- !asmFalseCommand

            curMicroComandIndex := !curMicroComandIndex + 1

            if (srcNode.InPorts.[1].Inputs.[0].Node.OpType = VSFG_TYPE)
            then    
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))     
                curMicroComandIndex := 0
                curGlobalComandIndex := !curGlobalComandIndex + 4
                asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))     
                this.Tta.ReleaseAllBuses()

        and GenerateCommandFromOperation(edge : Edge) = 
            let srcNode = edge.SrcNode
            let dstNode = edge.DstNode

            let (srcLn : int<ln>, srcCol : int<col>) = srcNode.ResultAddr
            let (dstLn : int<ln>, dstCol : int<col>) = dstNode.ResultAddr
            let srcPort = edge.OutPort.Index
            let dstPort = edge.InPort.Index

            let asmCommand = Mov((srcLn, srcCol, srcPort), (dstLn, dstCol, dstPort))
            asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmCommand

            curMicroComandIndex := !curMicroComandIndex + 1

        and GenerateCommand(edge : Edge) = 
            let srcNode = edge.SrcNode
            let dstNode = edge.DstNode

            if (dstNode.OpType = VSFG_TYPE)
            then
                GenerateCommandToVsfgType(edge)
            else
                if (srcNode.OpType = CONST_TYPE)
                then
                    GenerateCommandFromConstType(edge)
                elif (srcNode.OpType = MULTIPLEXOR_TYPE)
                then
                    GenerateCommandFromMultiplexorType(edge)
                else
                    GenerateCommandFromOperation(edge)

        (**
         * Move node's result address to the register (if TTA has at least 1 free register) 
         *)
        and SwapNodeWithRegister(node : Node) = 
            let isFreeRegister = this.Tta.IsFreeFU(REGISTER_TYPE)
            if isFreeRegister
            then
                let index = this.Tta.GetFreeFU(REGISTER_TYPE)
                let initNode = new InitialNode()
                initNode.ResultAddr <- index
                GenerateCommand(Edge(node.OutPorts.[0], initNode.InPorts.[0]))
                this.Tta.SetFUAsNonFree(index)
                this.Tta.SetFUAsFree(node.ResultAddr)
                node.ResultAddr <- index
                node.OpType <- REGISTER_TYPE
            
            isFreeRegister

        (** Remove all used edges and free TTA resources *)
        let ReleaseUsedEdgesAndFreeResources() = 
            let toRemove = readyEdges.FindAll(fun x -> x.IsUsed = true)
            
            for edge in toRemove do
                if edge.SrcNode.Status = Used && not (edge.SrcNode.OpType = CONST_TYPE) && not (edge.SrcNode.IsHaveDstMUX())
                then
                    this.Tta.SetFUAsFree(edge.SrcNode.ResultAddr)
             
            ignore(readyEdges.RemoveAll(fun x -> x.IsUsed = true))

        let AssociateNodePorts (node : Node) = 
            let index = ref 0<port>
            
            for port in node.InPorts do
                port.Index <- !index
                index := !index + 1<port>

            if (node.OpType = MULTIPLEXOR_TYPE || node.OpType = VSFG_TYPE)
            then
                ()
                //node.InPorts.[0].IsTrigger <- true
            else
                node.InPorts.[int (!index - 1<port>)].IsTrigger <- true

            for port in node.OutPorts do
                port.Index <- !index
                index := !index + 1<port>

            
        let Initialization() = 
            asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))
            
            for initNode in this.Vsfg.InitialNodes do
                readyEdges.AddRange(initNode.GetNextEdges())
            for constNode in this.Vsfg.ConstNodes do
                readyEdges.AddRange(constNode.GetNextEdges())

        let CaptureResource(node : Node) = 
            let index = this.Tta.GetFreeFU(node.OpType)
            node.ResultAddr <- index
            this.Tta.SetFUAsNonFree(index)
            

        Initialization()

        while readyEdges.Count <> 0 do

            let curReadiesLength = readyEdges.Count
            for tmp in [0..1] do
                for i in [0..curReadiesLength-1] do
                    let curEdge = readyEdges.[i]
                    let srcNode = curEdge.SrcNode
                    let dstNode = curEdge.DstNode

                    AssociateNodePorts(dstNode)

                    if (this.Tta.IsFreeBus() && not (curEdge.InPort.IsTrigger && curEdge.DstNode.inPortsCount > 1) && not (curEdge.IsUsed))
                    then
                        if (dstNode.Status = Unused)
                        then
                            let isHaveFreeFU = this.Tta.IsFreeFU(dstNode.OpType)

                            if isHaveFreeFU || dstNode = ((this.Vsfg.TerminalNodes).[0]) || dstNode.OpType = VSFG_TYPE
                            then

                                dstNode.DecInPorts()
                                srcNode.DecOutPorts()

                                if dstNode <> ((this.Vsfg.TerminalNodes).[0])
                                then 
                                    CaptureResource(dstNode)

                                    if dstNode.Status = Ready
                                    then    
                                        readyEdges.AddRange(dstNode.GetNextEdges())
                            
                                SetEdgeAsUsed(curEdge)

                                (**                                      
                                 * We shouldn't generate command for false and true branches in the multiplexor type.   
                                 *)
                                if (dstNode.OpType <> MULTIPLEXOR_TYPE || curEdge.InPort.Index = 0<port>)
                                then
                                    GenerateCommand(curEdge)
                                    this.Tta.TakeABus()

                        elif (dstNode.Status = Preparing)
                        then
                            srcNode.DecOutPorts()
                            dstNode.DecInPorts()

                            if dstNode.Status = Ready
                            then    
                                readyEdges.AddRange(dstNode.GetNextEdges())

                            SetEdgeAsUsed(curEdge)

                            (**                                      
                             * We shouldn't generate command for false and true branches in the multiplexor type.   
                             *)
                            if (dstNode.OpType <> MULTIPLEXOR_TYPE || curEdge.InPort.Index = 0<port>)
                            then
                                GenerateCommand(curEdge)
                                this.Tta.TakeABus()

                        else
                            printfn "ERROR OCCURRED"
            
            ReleaseUsedEdgesAndFreeResources()
            CheckEndOfInstruction()

        asmCode.RemoveAt(asmCode.Count - 1)
        asmCode

(*
[<EntryPoint>]
let main(arg : string[]) = 
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z
    "
        )
    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)
    inits.[2].ResultAddr <- (1<ln>, 2<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 3<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let FU3 = SUB("in1", "in2t", "out1", true)
    let FU4 = DIV("in1", "in2t", "out1", true)
    let TTA = new TTA([| (FU1, 5); (FU2, 5); (FU3, 5); (FU4, 5) |], 3)

    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    let file = new System.IO.StreamWriter(@"C:\Users\User\Documents\Workspace\test.txt")

    code.ForEach( 
        fun x -> 
        ( 
            //printf "("
            Array.iter( fun y -> file.Write(Asm.toString(y, TTA)); file.Write("; ")) x
            file.WriteLine()
            //printfn ")"
        ) 
    )

    file.Close()
    
    0
*)

(*
[<EntryPoint>]
let main(arg : string[]) = 

    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = if x < y + z
                                              then
                                                x + y / 1
                                              else
                                                x + y + z
                                              
    "
        )
    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)
    inits.[2].ResultAddr <- (1<ln>, 2<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 3<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let FU3 = BOOL("0", true)
    let FU4 = LT("in1", "in2t", "out1", true)
    let FU5 = DIV("in1", "in2t", "out1", true)
    let FU6 = PC("000", true)
    let TTA = new TTA([| (FU1, 2); (FU2, 10); (FU3, 5); (FU4, 5); (FU5, 1); (FU6, 1) |], 3)
    
    TTA.SetFUAsNonFree(inits.[0].ResultAddr)
    TTA.SetFUAsNonFree(inits.[1].ResultAddr)
    TTA.SetFUAsNonFree(inits.[2].ResultAddr)
    TTA.SetFUAsNonFree(terminals.[0].ResultAddr)
    
    let compiler = new VSFGCompiler(vsfg, TTA)

    let code = compiler.Compile()

    let file = new System.IO.StreamWriter(@"C:\Users\User\Documents\Workspace\test.txt")

    code.ForEach( 
        fun x -> 
        ( 
            //printf "("
            Array.iter( fun y -> file.Write(Asm.toString(y, TTA)); file.Write("; ")) x
            file.WriteLine()
            //printfn ")"
        ) 
    )

    file.Close()
    
    0
*)