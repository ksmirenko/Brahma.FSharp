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




type VSFGConstructor (input: string) =  
    let helper = new TypedTreeGetter(input)
    let visitor = visitExpr
    let functionMap = new Dictionary<string, Brahma.TTA.VSFG.VSFG>()

    new(input) = VSFGConstructor(input)

    
    member this.getVSFG (e:FSharpExpr) = 
        match e with 
            |BasicPatterns.Value v->
                   //if already have created
                   functionMap.[v.DisplayName]
            |_ ->
                let initials = new Dictionary<string, Node>()
                let terminal = new TerminalNode()
                let consts = new Dictionary<int, ConstNode>()

                let rec f (prev: Node) inputN (e:FSharpExpr) = 
                    match e with 
                    | BasicPatterns.Call(objExprOpt, memberOrFunc, typeArgs1, typeArgs2, argExprs) ->  
                        match memberOrFunc.DisplayName with 
                        | "( + )" -> 
                            let call = new AddNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs (call) 0  argExprs 
                            ()
                        | "( - )" -> 
                            let call = new SubNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs call 0  argExprs 
                            ()
                        | "( / )" -> 
                            let call = new DivNode()
                            VSFG.AddEdgeByInd (call) (0) (prev) (0)
                            visitArgs call 0 argExprs 
                            ()
                        | "( * )" -> 
                            let call = new DivNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs call 0 argExprs
                        | "( > )" -> 
                            let call = new GtNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs call 0 argExprs
            
                        | "( < )" -> 
                            printfn "to input port %A of object %A <- <" inputN prev 
                            let call = new LtNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs call 0 argExprs 
             
                        | "( <= )" -> 
                            let call = new LeqNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs call 0 argExprs
                        | "( >= )" -> 
                            let call = new GeqNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs call 0 argExprs
                        | "( = )" -> 
                            let call = new EqNode()
                            VSFG.AddEdgeByInd (call :> Node) (0) (prev) (0)
                            visitArgs call 0 argExprs 
                        | _ -> 
                            let nested = new NestedVsfgNode(functionMap.[memberOrFunc.DisplayName])
                            VSFG.AddEdgeByInd (nested :> Node) (0) (prev) (0)
                            visitArgs nested 0 argExprs
                            ()     

                    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) -> 
                        let mux = new MultiplexorNode()
                        VSFG.AddEdgeByInd (mux) 0 (prev) inputN
                        f mux 1  guardExpr
                        f mux 0  elseExpr
                        f mux 2  thenExpr

                        ()

                    | BasicPatterns.Let((bindingVar, bindingExpr), bodyExpr) ->  
                         let vsfg = this.getVSFG bindingExpr
                         functionMap.Add (bindingVar.DisplayName, vsfg)
                         let nested = new Brahma.TTA.VSFG.NestedVsfgNode(vsfg)
                         f prev inputN bodyExpr 
                         ()

                    | BasicPatterns.LetRec(recursiveBindings, bodyExpr) -> 
                         let name = (fst recursiveBindings.Head).DisplayName
                         let bindingExpr = snd recursiveBindings.Head
                         let vsfg = this.getVSFG bindingExpr

                         functionMap.Add (name, vsfg)
                         let nested = new Brahma.TTA.VSFG.NestedVsfgNode(vsfg)

                         f prev inputN  bodyExpr
                         ()
                    | BasicPatterns.Const(constValueObj, constType) ->
                        let value = Convert.ToInt32(constValueObj)
                        if consts.ContainsKey value then
                            VSFG.AddEdgeByInd consts.[value] 0 prev inputN
                        else
                            let constNode = new ConstNode(value)
                            consts.Add (value, constNode)
                            VSFG.AddEdgeByInd constNode 0 prev inputN
                        ()
                    | BasicPatterns.Value(valueToGet) ->
                
                        let name = valueToGet.DisplayName
                        if not (initials.ContainsKey name) && not (functionMap.ContainsKey name) then
                           let initial = InitialNode()
                           initials.Add (name, initial)
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
                        let nested = Brahma.TTA.VSFG.NestedVsfgNode (this.getVSFG(funcExpr))
                        
                        VSFG.AddEdgeByInd (nested) 0 (prev) inputN
                        visitArgs (nested) 0 argExprs
                        ()

            
                    | _ -> failwith (sprintf "unrecognized %+A" e)

                and visitArgs (prev: Node)  i  exprs =
                    match exprs with
                        | h::t -> 
                            f prev i  h
                            visitArgs prev (i+1)  t
                        | [] -> ()
        
                f terminal 0  e
                new  VSFG(initials.Values |>  Seq.toArray, [|terminal|], consts.Values |> Seq.toArray) 
            

    member this.print f = 
        (helper.getFSharpExpr 0) |> visitor f

    member this.Helper = helper




                            

    


  