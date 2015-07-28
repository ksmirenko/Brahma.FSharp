namespace Brahma.TTA.VSFGCompiler

open Brahma.TTA.VirtualTTA
open Brahma.TTA.VSFG
open Brahma.TTA.VSFGConstructor

type Addr = int<ln> * int<col> * int<port>

type AsmType =
    | Mov of Addr * Addr
    | Mvc of int * Addr

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


type VSFGCompiler(vsfg : VSFG, tta : TTA) = 
    let asmCode = new ResizeArray<AsmType array>()
    let readies = new ResizeArray<Node>()

    member this.Vsfg = vsfg
    member this.Tta = tta

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
            
        let GenerateCommand(fromNode : Node, toNode : Node) = 
            let (fromLn : int<ln>, fromCol : int<col>) = fromNode.ResultAddr
            let (toLn : int<ln>, toCol : int<col>) = toNode.ResultAddr
            let (fromPort : int<port>, toPort : int<port>) = fromNode.GetPorts(toNode)

            let asmComand = Mov((fromLn, fromCol, fromPort), (toLn, toCol, toPort))
            asmCode.[!curGlobalComandIndex].[!curMicroComandIndex] <- asmComand

            curMicroComandIndex := !curMicroComandIndex + 1


        let ReleaseUsedNodesFromReadies() =
            let toRemove = readies.FindAll(fun x -> x.Status = Used)
            toRemove.ForEach(fun x -> this.Tta.SetFUAsFree(x.ResultAddr)) 
            ignore(readies.RemoveAll(fun x -> x.Status = Used))  

        let IsWillGeneratedTriggerInstruction(srcNode : Node, dstNode : Node) = 
            let dstPortIndex = snd (srcNode.GetPorts(dstNode))
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

                    let dstNodes = srcNode.GetNextNotVisitedNodes()
                
                    for dstNode in dstNodes do
                        let portIndexes = srcNode.GetPorts(dstNode)
                        let dstPortIndex = snd portIndexes

                        if (this.Tta.IsFreeBus() && not (IsWillGeneratedTriggerInstruction(srcNode, dstNode)))
                        then
                            if dstNode.Status = Unused 
                            then
                                let isHaveFreeFU = this.Tta.IsFreeFU(dstNode.OpType)
                
                                if isHaveFreeFU
                                then
                                    let index = this.Tta.GetFreeFU(dstNode.OpType)
                                    this.Tta.SetFUAsNonFree(index)
                                    this.Tta.TakeABus()

                                    if dstNode <> ((this.Vsfg.TerminalNodes).[0])
                                    then dstNode.ResultAddr <- index

                                    srcNode.SetNodeAsVisited(dstNode)

                                    dstNode.DecInPorts()
                                    srcNode.DecOutPorts()

                                    if dstNode.Status = Ready
                                    then readies.Add(dstNode)

                                    GenerateCommand(srcNode, dstNode)

                            elif dstNode.Status = Preparing
                            then
                                srcNode.DecOutPorts()
                                dstNode.DecInPorts()
                                this.Tta.TakeABus()

                                if dstNode.Status = Ready
                                then readies.Add(dstNode)

                                srcNode.SetNodeAsVisited(dstNode)

                                GenerateCommand(srcNode, dstNode)
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

(*
[<EntryPoint>]
let main(arg : string[]) = 
    let t = VSFGConstructor("

    let main (x:int) (y:int) (z : int) :int = (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z + (x + y) + z + x + x + y + z + x + y + z / x + y / z
    "
        )
    let vsfg = t.getVSFG (t.Helper.getFSharpExpr 0)

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