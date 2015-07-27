namespace Brahma.TTA.VSFGCompiler

open Brahma.TTA.VirtualTTA
open Brahma.TTA.VSFG

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


        Initialization()        
         
        while not (readies.Count = 1 && readies.[0] = (this.Vsfg.TerminalNodes).[0]) do

            let curReadiesLength = readies.Count

            for i in  [0..curReadiesLength-1] do
                let srcNode = readies.[i]

                let dstNodes = srcNode.GetNextNotVisitedNodes()

                for dstNode in dstNodes do
                    if (this.Tta.IsFreeBus())
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
                        else
                            printfn "ERROR\n"

            ReleaseUsedNodesFromReadies()

            CheckEndOfInstruction()

        //printfn "%i (%i,%i)\n" readies.Count (int(fst readies.[0].ResultAddr)) (int(snd readies.[0].ResultAddr))
        asmCode.RemoveAt(asmCode.Count-1)

        asmCode 
