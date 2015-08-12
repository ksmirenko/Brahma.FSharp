module Brahma.TTA.VSFG.Visualization
open Brahma.TTA.VSFG
open Brahma.TTA.VirtualTTA
open System
open System.IO
open System.Collections.Generic


let VSFGtoDot (vsfg: VSFG) (file: string) =
        let inp = (file + ".dot")
        use file = new StreamWriter(inp) 

        let used = new ResizeArray<bool>()
        for i = 0 to 100000 do
            used.Add false
        let Q = new Queue<VSFG>()
        Q.Enqueue(vsfg)

        let rec f (n : Node) = 
            if used.[n.indexForDot] <> true then
                used.[n.indexForDot] <- true
                let E = n.GetNextEdges()
                for e in E do
                    let  a = NameForNode(e.SrcNode)
                    let mutable b = NameForNode(e.DstNode)
                     
                    file.WriteLine (sprintf "\"%s\" -> \"%s\"" a b)
                    f e.DstNode

         and NameForNode (n: Node) =
            match n.OpType with 
            | ADD_TYPE -> 
                (sprintf "+ %A LEFT: %A RIGHT: %A" n.indexForDot n.leftBalance n.rightBalance)
                //"+" + n.indexForDot.ToString()
            | SUB_TYPE ->
                "-" + n.indexForDot.ToString()
            | DIV_TYPE ->
                "/" + n.indexForDot.ToString()
            | MUL_TYPE -> 
                "*" + n.indexForDot.ToString()
            | CONST_TYPE ->
                "CONST: " + (n :?> ConstNode).Value.ToString()
            | REGISTER_TYPE ->
                try (n :?> InitialNode).Name
                with
                | :? System.InvalidCastException -> "OUT: " + (n :?> TerminalNode).Name
            | MULTIPLEXOR_TYPE -> 
                "MUX " + n.indexForDot.ToString()
            | VSFG_TYPE-> 

                let a = (n :?> NestedVsfgNode).Vsfg
                Q.Enqueue(a)
                "CALL: " + a.Name

            | EQ_TYPE -> 
                "=" + n.indexForDot.ToString()
            | LEQ_TYPE ->
                 "<=" + n.indexForDot.ToString()
            | GEQ_TYPE ->
                 ">=" + n.indexForDot.ToString()
            | LQ_TYPE ->
                 "<" + n.indexForDot.ToString()
            | GQ_TYPE ->
                 ">" + n.indexForDot.ToString()
            | _ -> "WTF?" 

        let printVSFG(vsfg: VSFG) = 

            file.WriteLine (sprintf "digraph %A {" vsfg.Name)
            for i = 0 to vsfg.InitialNodes.Length - 1 do
                f  vsfg.InitialNodes.[i] 
        
            for i = 0 to vsfg.ConstNodes.Length - 1 do
                f  vsfg.ConstNodes.[i] 

            file.WriteLine "}"
            ()

        
        while Q.Count <> 0 do
            printVSFG(Q.Dequeue())

        ()

let VSFGtoPNG (vsfg: VSFG) (file: string) = 
     let startInfo = new System.Diagnostics.ProcessStartInfo("../../../../dot.exe")
     VSFGtoDot vsfg file
     startInfo.Arguments <- (sprintf "-Tpng %A.dot -o %A.png" file file)
     startInfo.UseShellExecute <- false
     startInfo.CreateNoWindow <- true

     System.Diagnostics.Process.Start(startInfo) |> ignore
     ()
         

