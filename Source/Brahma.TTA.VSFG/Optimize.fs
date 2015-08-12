module Brahma.TTA.VSFG.Optimize

open Brahma.TTA.VSFG 
open Brahma.TTA.VirtualTTA
type BalanceStatus = Left | Right | Neitral
let rebuildPlus (vsfg: VSFG) = 
    let balanceDiff (n: Node) = abs(n.leftBalance - n.rightBalance)
    let getBalanceStatus (n: Node) =
        if n.leftBalance - n.rightBalance > 1 then
            Left
        else if n.rightBalance - n.leftBalance > 1 then
            Right
        else 
            Neitral

    let getDownNode (n: Node) = n.GetNextNodes().[0] 
    let getUpLeftNode (n: Node) = n.GetPrevNodes().[0] 
    let getUpRightNode (n: Node) = n.GetPrevNodes().[1] 

    let tryDown (n: Node) = 
        if (n.GetNextNodes().Count = 0 || n.GetNextNodes().[0].OpType <> ADD_TYPE) then 
            false 
        else 
            true
    let tryDoubleDown (n: Node) = 
        if tryDown n then
            if n |> getDownNode |> tryDown then
                true
            else false
        else false

    let tryUpLeft (n: Node) = 
        if (n.GetPrevNodes().Count = 0 || n.GetPrevNodes().[0].OpType <> ADD_TYPE) then 
            false 
        else 
            true

    let tryUpRight (n: Node) = 
        if (n.GetPrevNodes().Count = 0 || n.GetPrevNodes().[1].OpType <> ADD_TYPE) then 
            false 
        else 
            true

    let rec balance (n : Node) = 
        let diff = (n |> balanceDiff)
        match n |> getBalanceStatus with
        | Left -> 
            let mutable leftNode = getLeftNode n diff
            while (tryDoubleDown leftNode) do
                leftNode <- fixLeftBalance leftNode n
            balance leftNode
            ()
        | Right ->
            let mutable rightNode = getRightNode n diff
            while (tryDoubleDown rightNode) do
                rightNode <- fixRightBalance rightNode n
            balance rightNode
            
            ()
        | Neitral -> ()
    and getLeftNode (n:Node) count =
        if count = 0 then
            n
        else getLeftNode (getUpLeftNode n) (count - 1)

    and getRightNode (n:Node) count =
        if count = 0 then
            n
        else getRightNode (getUpRightNode n) (count - 1)

    and fixLeftBalance (n: Node) (t: Node) = 
        let t2 = getDownNode n
        let t3 = getDownNode t2
        let t3Parent = getUpRightNode t3
        VSFG.RemoveEdge n.OutPorts.[0] t2.InPorts.[0]
        VSFG.RemoveEdge t3Parent.OutPorts.[0] t3.InPorts.[1]
        VSFG.RemoveEdge t2.OutPorts.[0] t3.InPorts.[0]

        VSFG.AddEdge t2.OutPorts.[0] t3.InPorts.[1]
        t3.rightBalance <- t2.rightBalance + 1

        VSFG.AddEdge n.OutPorts.[0] t3.InPorts.[0]
        VSFG.AddEdge t3Parent.OutPorts.[0] t2.InPorts.[0]
        t2.leftBalance <- t3Parent.leftBalance + 1
        siftLeftBalance t2 t

        getDownNode n

    and fixRightBalance (n: Node) (t: Node) = 
        let t2 = getDownNode n
        let t3 = getDownNode t2
        let t3Parent = getUpLeftNode t3
        VSFG.RemoveEdge n.OutPorts.[0] t2.InPorts.[1]
        VSFG.RemoveEdge t3Parent.OutPorts.[0] t3.InPorts.[0]
        VSFG.RemoveEdge t2.OutPorts.[0] t3.InPorts.[1]

        VSFG.AddEdge t2.OutPorts.[0] t3.InPorts.[0]
        t3.leftBalance <- t2.leftBalance + 1

        VSFG.AddEdge n.OutPorts.[0] t3.InPorts.[1]
        VSFG.AddEdge t3Parent.OutPorts.[0] t2.InPorts.[1]
        t2.rightBalance <- t3Parent.rightBalance + 1
        siftRightBalance t2 t

        getDownNode n
        

    and siftLeftBalance (n: Node) (t: Node) = 
        if tryDown n && n.indexForDot <> t.indexForDot then
            let temp = (getDownNode n)
            temp.leftBalance <- temp.leftBalance - 1
            siftLeftBalance temp t

    and siftRightBalance (n: Node) (t: Node) = 
         if tryDown n && n.indexForDot <> t.indexForDot then
            let temp = (getDownNode n)
            temp.rightBalance <- temp.rightBalance - 1
            siftRightBalance temp t

    for e in vsfg.AddNodeReady do
        balance e

    vsfg

