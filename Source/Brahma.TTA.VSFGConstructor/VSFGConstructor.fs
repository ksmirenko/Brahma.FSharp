module Brahma.TTA.VSFGConstructor
open System.IO
open Brahma.TTA.VSFG
open Brahma.TTA.VSFGConstructorHelper
open System
open System.Collections.Generic
open Brahma.TTA.VirtualTTA

open Microsoft.FSharp.Compiler.SourceCodeServices

let rec visitExpr f (e:FSharpExpr) = 
    f e
    match e with 
    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) -> 
        visitObjArg f objExprOpt; visitExprs f argExprs
    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
        visitExpr f guardExpr; visitExpr f thenExpr; visitExpr f elseExpr
    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) -> 
        visitExpr f bindingExpr; visitExpr f bodyExpr
    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
        List.iter (snd >> visitExpr f) recursiveBindings; visitExpr f bodyExpr
    | BasicPatterns.Const(constValueObj, constType) -> ()
    | BasicPatterns.Value(valueToGet) -> ()
    | BasicPatterns.Lambda(lambdaVar, bodyExpr) -> 
       visitExpr f bodyExpr
    | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> 
        visitExpr f funcExpr; visitExprs f argExprs
    | _ -> failwith (sprintf "unrecognized %+A" e)

and visitExprs f exprs = 
    List.iter (visitExpr f) exprs

and visitObjArg f objOpt = 
    Option.iter (visitExpr f) objOpt


let copy<'a> (t: Dictionary<string, 'a>) =
     let temp = new Dictionary<string, 'a>()
     for e in t do
        temp.Add (e.Key, e.Value) |> ignore
     temp
        


type VSFGConstructor (input: string) =  
    let helper = new TypedTreeGetter(input)
    let visitor = visitExpr
    let index = ref 0
    let initialsDEBUG = new Dictionary<string, Node>()
    
    new(input) = VSFGConstructor(input)
    
    member private this.getVSFG2 (e:FSharpExpr) (name: string) (functionMap: Dictionary<string, VSFG>)  = 
        let vsfg = functionMap.[name]

        let initials =
            if initialsDEBUG.Count <> 0 then
               let e = copy initialsDEBUG 
               initialsDEBUG.Clear()
               e
            else  
                new Dictionary<string, Node>()       

        let terminal = new TerminalNode()
        terminal.indexForDot <- index.Value
        incr index

        vsfg.SetTerminalNodes([|terminal|])

        if initials.Count <> 0 then
            vsfg.SetInitialNodes(initials.Values |> Seq.toArray)

        let consts = new Dictionary<int, ConstNode>()
        
        let rec f (prev: Node) inputN (e:FSharpExpr) =  
            match e with 
            | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->  
                match memberOrFunc.DisplayName with 
                | "( + )" -> 
                    let call = new AddNode()
                    call.indexForDot <-index.Value
                    incr index
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs (call) 0  argExprs 
                    ()
                | "( - )" -> 
                    let call = new SubNode()
                    call.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0  argExprs 
                    ()
                | "( / )" -> 
                    let call = new DivNode()

                    call.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs 
                    ()
                | "( * )" -> 
                    let call = new MulNode()

                    call.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs

                | "( > )" -> 
                    let call = new GtNode()

                    call.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs
            
                | "( < )" -> 
                    let call = new LtNode()

                    call.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs 
             
                | "( <= )" -> 
                    let call = new LeqNode()

                    call.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs
                | "( >= )" -> 
                    let call = new GeqNode()

                    call.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs
                | "( = )" -> 
                    let call = new EqNode()
                    call.indexForDot <- index.Value
                    incr index
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs 
                | _ -> 
                    let nested = new NestedVsfgNode(functionMap.[memberOrFunc.DisplayName])
                    
                    nested.indexForDot <- index.Value
                    incr index

                    VSFG.AddEdgeByInd (nested ) (0) (prev) (inputN)
                    visitArgs nested 0 argExprs
                    ()     

            | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
                let mux = new MultiplexorNode()
                mux.indexForDot <- index.Value
                incr index
                match prev.OpType with  
                    |MULTIPLEXOR_TYPE -> 
                        let initial = new InitialNode()
                        initial.indexForDot <- index.Value
                        initial.Name <- "MUXTEMP" + index.Value.ToString()
                        incr index
                        
                        VSFG.AddEdgeByInd mux 0 initial 0
                        VSFG.AddEdgeByInd (initial) 0 (prev) inputN
                    | _ -> 
                        VSFG.AddEdgeByInd (mux) 0 (prev) inputN

                f mux 0  guardExpr
                f mux 1  elseExpr
                f mux 2  thenExpr

                ()

            | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->  
                    let name = bindingVar.DisplayName
                    if (functionMap.ContainsKey name) then
                        functionMap.Remove name |> ignore
                    let tempVsfg = new VSFG([||], [||], [||])
                    tempVsfg.Name <- name
                    functionMap.Add (name, tempVsfg)

                    let vsfg = this.getVSFG2 bindingExpr name (copy functionMap)
                    let nested = new Brahma.TTA.VSFG.NestedVsfgNode(vsfg)

                    nested.indexForDot <-index.Value
                    incr index
                    f prev inputN bodyExpr 
                    ()

            | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
                    
                    let name = (fst recursiveBindings.Head).DisplayName

                    let bindingExpr = snd recursiveBindings.Head

                    if (functionMap.ContainsKey name) then
                        functionMap.Remove name |> ignore
                    let tempVsfg = new VSFG([||], [||], [||])

                    tempVsfg.Name <- name
                    functionMap.Add (name, tempVsfg)
                    let vsfg = this.getVSFG2 bindingExpr name (copy functionMap)

                
                    let nested = new NestedVsfgNode(vsfg)
                    nested.indexForDot <-index.Value
                    incr index
                    f prev inputN  bodyExpr
                    ()

            | BasicPatterns.Const(constValueObj, constType) ->

                let value = Convert.ToInt32(constValueObj)
                if consts.ContainsKey value then
                    VSFG.AddEdgeByInd consts.[value] 0 prev inputN
                else
                    let constNode = new ConstNode(value)
                    constNode.indexForDot <-index.Value
                    incr index
                    consts.Add (value, constNode)
                    VSFG.AddEdgeByInd constNode 0 prev inputN
                ()

            | BasicPatterns.Value(valueToGet) ->

                let name = valueToGet.DisplayName
                if not (initials.ContainsKey name) && not (functionMap.ContainsKey name) then
                    let initial = new InitialNode() 
                    initial.indexForDot <- index.Value
                    incr index
                    initial.Name <- name

                    initials.Add (name, initial)
                    VSFG.AddEdgeByInd (initials.[name]) 0 (prev) inputN

                else 
                    if initials.ContainsKey name then 
                        VSFG.AddEdgeByInd (initials.[name]) 0 (prev) inputN
                    else 
                        VSFG.AddEdgeByInd (functionMap.[name].TerminalNodes.[0]) 0 (prev) inputN

            | BasicPatterns.Lambda(lambdaVar, bodyExpr) ->
 
                let name = lambdaVar.DisplayName
                if not (initials.ContainsKey name) && not (functionMap.ContainsKey name) then
                    let initial = InitialNode()
                    initials.Add (name, initial)

                f prev inputN  bodyExpr
            | BasicPatterns.Application(funcExpr, typeArgs, argExprs) -> 
                
                match funcExpr with
                    |BasicPatterns.Value v -> 
                        let vsfg = functionMap.[v.DisplayName] 

                        if vsfg.InitialNodes.Length = 0 then 
                            vsfg.SetInitialNodes (initials.Values |> Seq.toArray)
                            vsfg.SetTerminalNodes [|terminal|]
                        
                        let nested = new Brahma.TTA.VSFG.NestedVsfgNode (vsfg)
                        VSFG.AddEdgeByInd (nested) 0 (prev) inputN         

                        visitArgs (nested) 0 argExprs

                        ()
                    |_ -> failwith (sprintf "unrecognized %+A" e)
                ()

            
            | _ -> failwith (sprintf "unrecognized %+A" e)

        and visitArgs (prev: Node)  i  exprs =
            match exprs with
                | h::t -> 
                    f prev i  h
                    visitArgs prev (i+1)  t
                | [] -> ()
        
        for e in functionMap do
            ()
        f terminal 0  e
        vsfg.SetInitialNodes (initials.Values |> Seq.toArray)
        vsfg.SetConstNodes (consts.Values |> Seq.toArray)
        
        vsfg
               
    member this.getVSFG = 
        let vsfg = new VSFG([||], [||], [||])
        let name = (helper.getFSharpFunctionalSymbol 0).DisplayName
        vsfg.Name <- name
        let t = helper.getFSharpArgs 0
        for i = 0 to (helper.getFSharpArgs 0).Length - 1 do
            let initial = new InitialNode()
            initial.indexForDot <- index.Value
            incr index
            let name = t.[i].[0].DisplayName
            initial.Name <- name 
            initialsDEBUG.Add (name, initial)

           
        let functionMap = new Dictionary<string, VSFG>()
        functionMap.Add (name, vsfg)
        this.getVSFG2 (helper.getFSharpExpr 0) name functionMap

    member this.print f = 
        (helper.getFSharpExpr 0) |> visitor f

    member this.Helper = helper

    static member VSFGtoDot (vsfg: VSFG) (file: string) =

        use file = new StreamWriter(file)
        
        let used = new ResizeArray<bool>()
        for i = 0 to 1000 do
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
                "+" + n.indexForDot.ToString()
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



       
            
    

                            

    


  