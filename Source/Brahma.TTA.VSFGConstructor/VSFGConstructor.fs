module Brahma.TTA.VSFGConstructor

open Brahma.TTA.VSFG
open Brahma.TTA.VSFGConstructorHelper
open System
open System.Collections.Generic

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


let copy (t: Dictionary<string, VSFG>) =
     let temp = new Dictionary<string, VSFG>()
     for e in t do
        temp.Add (e.Key, e.Value) |> ignore
     temp
        


type VSFGConstructor (input: string) =  
    let helper = new TypedTreeGetter(input)
    let visitor = visitExpr

    new(input) = VSFGConstructor(input)

    
    member private this.getVSFG2 (e:FSharpExpr) (name: string) (functionMap: Dictionary<string, VSFG>)  = 
       
        let initials = new Dictionary<string, Node>()
        let terminal = new TerminalNode()
        let consts = new Dictionary<int, ConstNode>()
        let vsfg = functionMap.[name]

        let rec f (prev: Node) inputN (e:FSharpExpr) =  
            match e with 
            | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->  
                match memberOrFunc.DisplayName with 
                | "( + )" -> 
                    let call = new AddNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs (call) 0  argExprs 
                    ()
                | "( - )" -> 
                    let call = new SubNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0  argExprs 
                    ()
                | "( / )" -> 
                    let call = new DivNode()
                    VSFG.AddEdgeByInd (call) (0) (prev) (inputN)
                    visitArgs call 0 argExprs 
                    ()
                | "( * )" -> 
                    let call = new DivNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs
                | "( > )" -> 
                    let call = new GtNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs
            
                | "( < )" -> 
                    printfn "to input port %A of object %A <- <" inputN prev 
                    let call = new LtNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs 
             
                | "( <= )" -> 
                    let call = new LeqNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs
                | "( >= )" -> 
                    let call = new GeqNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs
                | "( = )" -> 
                    let call = new EqNode()
                    VSFG.AddEdgeByInd (call :> Node) (0) (prev) (inputN)
                    visitArgs call 0 argExprs 
                | _ -> 
                    let nested = new NestedVsfgNode(functionMap.[memberOrFunc.DisplayName])
                    VSFG.AddEdgeByInd (nested :> Node) (0) (prev) (inputN)
                    visitArgs nested 0 argExprs
                    ()     

            | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
                printfn "IF "

                let mux = new MultiplexorNode()
                match prev.OpType with  
                    |MULTIPLEXOR_TYPE -> 
                        let initial = new InitialNode()
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
                    functionMap.Add (name, new VSFG([||], [||], [||]))

                    let vsfg = this.getVSFG2 bindingExpr name (copy functionMap)
                    let nested = new Brahma.TTA.VSFG.NestedVsfgNode(vsfg)
                    f prev inputN bodyExpr 
                    ()

            | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
                    
                    let name = (fst recursiveBindings.Head).DisplayName
                    printfn "Func %A " name

                    let bindingExpr = snd recursiveBindings.Head

                    if (functionMap.ContainsKey name) then
                        functionMap.Remove name |> ignore

                    functionMap.Add (name, new VSFG([||], [||], [||]))
                    let vsfg = this.getVSFG2 bindingExpr name (copy functionMap)

                
                    let nested = new NestedVsfgNode(vsfg)

                    f prev inputN  bodyExpr
                    ()

            | BasicPatterns.Const(constValueObj, constType) ->
                printfn "const "

                let value = Convert.ToInt32(constValueObj)
                if consts.ContainsKey value then
                    VSFG.AddEdgeByInd consts.[value] 0 prev inputN
                else
                    let constNode = new ConstNode(value)
                    consts.Add (value, constNode)
                    VSFG.AddEdgeByInd constNode 0 prev inputN
                ()
            | BasicPatterns.Value(valueToGet) ->
                printfn "val "

                let name = valueToGet.DisplayName
                if not (initials.ContainsKey name) && not (functionMap.ContainsKey name) then
                    let initial = InitialNode()
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
                        printf "norn"
                        VSFG.AddEdgeByInd (nested) 0 (prev) inputN
                        printf "norn"           

                        visitArgs (nested) 0 argExprs
                        printf "norn"

                        ()
                    |_ -> failwith (sprintf "unrecognized %+A" e)
                ()

            
            | _ -> failwith (sprintf "unrecognized %+A" e)

        and visitArgs (prev: Node)  i  exprs =
            printfn "%A" exprs
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
        vsfg.SetTerminalNodes [|terminal|]
        vsfg
               
    member this.getVSFG = 
        let vsfg = new VSFG([||], [||], [||])
        let name = (helper.getFSharpFunctionalSymbol 0).DisplayName
        let functionMap = new Dictionary<string, VSFG>()
        functionMap.Add (name, vsfg)
        this.getVSFG2 (helper.getFSharpExpr 0) name functionMap

    member this.print f = 
        (helper.getFSharpExpr 0) |> visitor f

    member this.Helper = helper


    

                            

    


  