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
        let curGlobalComandIndex = ref 0
        let curMicroComandIndex = ref 0

        let rec CheckEndOfInstruction() =
            let isFound = ref false

            if !curMicroComandIndex = 0
            then
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
                            SwapNodeWithRegister(prevNode)

            if !isFound || !curMicroComandIndex <> 0
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

        and GenerateCommand(edge : Edge) = 
            let srcNode = edge.SrcNode
            let dstNode = edge.DstNode

            if (srcNode.OpType = CONST_TYPE)
            then
                let (dstLn : int<ln>, dstCol : int<col>) = dstNode.ResultAddr
                let dstPort = edge.InPort.Index

                let asmCommand = Mvc((srcNode :?> ConstNode).Value, (dstLn, dstCol, dstPort))
                asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmCommand

                curMicroComandIndex := !curMicroComandIndex + 1

            elif (srcNode.OpType = MULTIPLEXOR_TYPE)
            then
                let predicateEdge = Edge(srcNode.InPorts.[0].Inputs.[0], srcNode.InPorts.[0])
                let falseEdge = (Edge(srcNode.InPorts.[1].Inputs.[0], srcNode.InPorts.[1]))
                let trueEdge =(Edge(srcNode.InPorts.[2].Inputs.[0], srcNode.InPorts.[2]))

                let srcPortPredicate = 0<port>
                let srcPortFalse = falseEdge.OutPort.Index
                let srcPortTrue = trueEdge.OutPort.Index

                let dstPort = edge.InPort.Index

                let (srcLnPredicate : int<ln>, srcColPredicate : int<col>) = edge.SrcNode.ResultAddr
                let (srcLnFalse : int<ln>, srcColFalse : int<col>) = falseEdge.SrcNode.ResultAddr
                let (srcLnTrue : int<ln>, srcColTrue : int<col>) = trueEdge.SrcNode.ResultAddr

                let (dstLn : int<ln>, dstCol : int<col>) = edge.DstNode.ResultAddr

                let asmTrueCommand = ref (Mov(Asm.emptyAddr, Asm.emptyAddr))

                if (srcNode.InPorts.[2].Inputs.[0].Node.OpType = CONST_TYPE)
                then
                    let constNode = (srcNode.InPorts.[2].Inputs.[0].Node :?> ConstNode)
                    asmTrueCommand := IfTrueConst((srcLnPredicate, srcColPredicate, srcPortPredicate), constNode.Value, (dstLn, dstCol, dstPort))
                else 
                    asmTrueCommand := IfTrue((srcLnPredicate, srcColPredicate, srcPortPredicate), (srcLnTrue, srcColTrue, srcPortTrue), (dstLn, dstCol, dstPort))
                    
                asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- !asmTrueCommand


                curMicroComandIndex := !curMicroComandIndex + 1
                this.Tta.TakeABus()


                if (not (this.Tta.IsFreeBus()))
                then
                    CheckEndOfInstruction()


                let asmFalseCommand = ref (Mov(Asm.emptyAddr, Asm.emptyAddr))

                if (srcNode.InPorts.[1].Inputs.[0].Node.OpType = CONST_TYPE)
                then
                    let constNode = (srcNode.InPorts.[1].Inputs.[0].Node :?> ConstNode)
                    asmFalseCommand := IfFalseConst((srcLnPredicate, srcColPredicate, srcPortPredicate), constNode.Value, (dstLn, dstCol, dstPort))
                else 
                    asmFalseCommand := IfFalse((srcLnPredicate, srcColPredicate, srcPortPredicate), (srcLnFalse, srcColFalse, srcPortFalse), (dstLn, dstCol, dstPort))

                asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- !asmFalseCommand

                curMicroComandIndex := !curMicroComandIndex + 1

            else
                let (srcLn : int<ln>, srcCol : int<col>) = srcNode.ResultAddr
                let (dstLn : int<ln>, dstCol : int<col>) = dstNode.ResultAddr
                let srcPort = edge.OutPort.Index
                let dstPort = edge.InPort.Index

                let asmCommand = Mov((srcLn, srcCol, srcPort), (dstLn, dstCol, dstPort))
                asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmCommand

                curMicroComandIndex := !curMicroComandIndex + 1

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


        let ReleaseUsedEdges() = 
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

            if (node.OpType = MULTIPLEXOR_TYPE)
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

                            if isHaveFreeFU || dstNode = ((this.Vsfg.TerminalNodes).[0])
                            then

                                dstNode.DecInPorts()
                                srcNode.DecOutPorts()

                                (* Когда будем обрабатывать хвостовую рекурсию, здесь пометим, что <> VSFG_TYPE *)
                                if dstNode <> ((this.Vsfg.TerminalNodes).[0])
                                then 
                                    let index = this.Tta.GetFreeFU(dstNode.OpType)
                                    AssociateNodePorts(dstNode)
                                    dstNode.ResultAddr <- index
                                    this.Tta.SetFUAsNonFree(index)

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
            
            ReleaseUsedEdges()
            CheckEndOfInstruction()

        asmCode.RemoveAt(asmCode.Count-1)
        asmCode
             

    (*
    member this.Compile() =  
        let curGlobalComandIndex = ref 0
        let curMicroComandIndex = ref 0


        let CheckEndOfInstruction() = 
            curMicroComandIndex := 0
            curGlobalComandIndex := !curGlobalComandIndex + 1
            this.Tta.ReleaseAllBuses()
            asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))

        let Initialization() = 
            asmCode.Add(Array.init (this.Tta.BusCount()) (fun x -> Mov(Asm.emptyAddr, Asm.emptyAddr)))
            readies.AddRange(this.Vsfg.InitialNodes)
            
        let GenerateCommand(fromNode : Node, toNodeIndex : int) = 
            let dstNodes = fromNode.GetNextNodes()
            let toNode = dstNodes.[toNodeIndex]

            if fromNode.OpType = MULTIPLEXOR_TYPE
            then
                let prevNodes = fromNode.GetPrevNodes()
                let predicateNode = prevNodes.[0]
                let falseNode = prevNodes.[1]
                let trueNode = prevNodes.[2]

                let (toLn : int<ln>, toCol : int<col>) = toNode.ResultAddr
                let (_ : int<port>, toPort : int<port>) = fromNode.GetPortsByNode(toNode)
                
                let (falsePort : int<port>, _ : int<port>) = falseNode.GetPortsByNode(fromNode)
                let (truePort : int<port>, _ : int<port>) = trueNode.GetPortsByNode(fromNode)

                let (fromLnTrue : int<ln>, fromColTrue : int<col>) = trueNode.ResultAddr
                let (fromLnFalse : int<ln>, fromColFalse : int<col>) = falseNode.ResultAddr

                let predicateAddr = (fst fromNode.ResultAddr, snd fromNode.ResultAddr, 0<port>)

                //Here we should generate 2 commands: 1 for true branch && 1 for false branch
                let asmTrueComand = IfTrue(predicateAddr, (fromLnTrue, fromColTrue, truePort), (toLn, toCol, toPort))
                asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmTrueComand

                curMicroComandIndex := !curMicroComandIndex + 1

                if (!curMicroComandIndex >= this.Tta.BusCount())
                then
                    curMicroComandIndex := 0
                    curGlobalComandIndex := !curGlobalComandIndex + 1

                let asmFalseComand = IfFalse(predicateAddr, (fromLnFalse, fromColFalse, falsePort), (toLn, toCol, toPort))
                asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmFalseComand

                curMicroComandIndex := !curMicroComandIndex + 1

            else
                let (fromLn : int<ln>, fromCol : int<col>) = fromNode.ResultAddr
                let (toLn : int<ln>, toCol : int<col>) = toNode.ResultAddr
                let (fromPort : int<port>, toPort : int<port>) = fromNode.GetPortsByNode(toNode)

                let asmComand = Mov((fromLn, fromCol, fromPort), (toLn, toCol, toPort))
                asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmComand

                curMicroComandIndex := !curMicroComandIndex + 1


        let ReleaseUsedNodesFromReadies() =
            let toRemove = readies.FindAll(fun x -> x.Status = Used)
            (* Don't free FU, which go have MUX successor *)
            toRemove.ForEach(fun x -> if not (x.IsHaveDstMUX()) then this.Tta.SetFUAsFree(x.ResultAddr)) 
            ignore(readies.RemoveAll(fun x -> x.Status = Used))  

        let IsWillGeneratedTriggerInstruction(srcNode : Node, dstNode : Node) = 
            let dstPortIndex = snd (srcNode.GetPortsByNode(dstNode))
            dstNode.IsTriggerPort(dstPortIndex) && dstNode.inPortsCount > 1
            
        (* Move all triggered instructions in the end of the array *)
        let ReorderDstNodes(srcNode : Node, dstNodes : ResizeArray<Node>) = 
            let l = ref 0
            let r = ref (dstNodes.Count - 1)
            while !l < !r do  
                if IsWillGeneratedTriggerInstruction(srcNode, dstNodes.[!l])
                then
                    let tmp = dstNodes.[!l]
                    dstNodes.[!l] <- dstNodes.[!r]
                    dstNodes.[!r] <- tmp
                    r := !r - 1
                l := !l + 1

        Initialization()        
         
        while not (readies.Count = 1 && readies.[0] = (this.Vsfg.TerminalNodes).[0]) do

            let curReadiesLength = readies.Count
            (* double pass through the loop is need to increase asm instruction density *)
            for tmp in [0..1] do
                for i in  [0..curReadiesLength-1] do
                    let srcNode = readies.[i]

                    let dstNodes = srcNode.GetNextNodes()
                
                    for dstNodeIndex in [0..dstNodes.Count-1] do
                        let dstNode = dstNodes.[dstNodeIndex]

                        let portIndexes = srcNode.GetPortsByNode(dstNode)
                        let dstPortIndex = snd portIndexes

                        if (this.Tta.IsFreeBus() && not (IsWillGeneratedTriggerInstruction(srcNode, dstNode))) && srcNode.IsVisited.[dstNodeIndex] = false
                        then
                            if dstNode.Status = Unused 
                            then
                                let isHaveFreeFU = this.Tta.IsFreeFU(dstNode.OpType)
                
                                if isHaveFreeFU
                                then
                                    let index = this.Tta.GetFreeFU(dstNode.OpType)
                                    this.Tta.SetFUAsNonFree(index)
                                    
                                    if dstNode <> ((this.Vsfg.TerminalNodes).[0])
                                    then dstNode.ResultAddr <- index

                                    dstNode.DecInPorts()
                                    srcNode.DecOutPorts()

                                    if dstNode.Status = Ready
                                    then readies.Add(dstNode)

                                    (**                                      
                                     * We shouldn't generate command for false and true branches in the multiplexor type.   
                                     *)
                                    if (dstNode.OpType <> MULTIPLEXOR_TYPE || dstPortIndex = 0<port>)
                                    then
                                        GenerateCommand(srcNode, dstNodeIndex)
                                        this.Tta.TakeABus() 

                                    srcNode.SetNodeAsVisited(dstNode)

                            elif dstNode.Status = Preparing
                            then
                                srcNode.DecOutPorts()
                                dstNode.DecInPorts()
                                
                                if dstNode.Status = Ready
                                then readies.Add(dstNode)

                                (**                                      
                                 * We shouldn't generate command for false and true branches in the multiplexor type.   
                                 *)
                                if (dstNode.OpType <> MULTIPLEXOR_TYPE || dstPortIndex = 0<port>)
                                then
                                    GenerateCommand(srcNode, dstNodeIndex)
                                    this.Tta.TakeABus()

                                srcNode.SetNodeAsVisited(dstNode)

                            elif dstNode.Status = Ready
                            then
                                ()
                            else
                                printfn "ERROR\n"

            ReleaseUsedNodesFromReadies()

            CheckEndOfInstruction()

        //printfn "%i (%i,%i)\n" readies.Count (int(fst readies.[0].ResultAddr)) (int(snd readies.[0].ResultAddr))
        asmCode.RemoveAt(asmCode.Count-1)

        asmCode 
        *)

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
    
    (*TODO: Fix bug with multiple if-then-else. Should compute before*)
    let t = VSFGConstructor("

    let main (x:int) (y : int) :int = if x + y + 1 < 1 then 30 else 120
    "
    )

    let vsfg = t.getVSFG

    let inits = vsfg.InitialNodes
    inits.[0].ResultAddr <- (1<ln>, 0<col>)
    inits.[1].ResultAddr <- (1<ln>, 1<col>)

    let terminals = vsfg.TerminalNodes
    terminals.[0].ResultAddr <- (1<ln>, 2<col>)

    let FU1 = ADD("in1", "in2t", "out1", true)
    let FU2 = REGISTER("0", true)
    let FU3 = BOOL("0", true)
    let FU4 = LT("in1", "in2t", "out1", true)
    let FU5 = DIV("in1", "in2t", "out1", true)
    let TTA = new TTA([| (FU1, 2); (FU2, 5); (FU3, 2); (FU4, 2); (FU5, 1) |], 3)
    
    TTA.SetFUAsNonFree(inits.[0].ResultAddr)
    TTA.SetFUAsNonFree(inits.[1].ResultAddr)
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