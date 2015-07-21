module Brahma.TTA.VirtualTTA.Tests

open NUnit.Framework

[<Test>]
let oneFU () = 
    let FU = ADD(0<inPort>, 0<inPort>, true)
    let TTA = new TTA([| (FU, 1) |], 1)
    
    (* Information about function unit doesn't matter, matters just type of FU (ex: ADD, SUB, etc..) 
     * So, we can write TTA.IsFreeFU(ADD(13<inPort>, 16<inPort>, false))
     *)
    let isFreeFU = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsTrue(isFreeFU)

    let ind = TTA.GetFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsTrue((ind = (0,0)))

    TTA.SetFUAsNonEmpty(ind)
    let isFreeFU = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsFalse(isFreeFU)

    TTA.SetFUAsEmpty(ind)
    let isNonFreeFU = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsTrue(isNonFreeFU)

[<Test>]
let twoSameFU() = 
    let FU = ADD(0<inPort>, 0<inPort>, true)
    let TTA = new TTA([| (FU, 2) |], 1)

    (* Check first FU *)
    let isFreeFU0 = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsTrue(isFreeFU0)
    let ind0 = TTA.GetFreeFU(ADD(0<inPort>, 0<inPort>, true))
    TTA.SetFUAsNonEmpty(ind0)

    (* Check second FU *)
    let isFreeFU1 = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsTrue(isFreeFU1)
    let ind1 = TTA.GetFreeFU(ADD(0<inPort>, 0<inPort>, true))
    TTA.SetFUAsNonEmpty(ind1)

    (* All FUs are not free *)
    let isAnybodyFree = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsFalse(isAnybodyFree)

    (* Free one FU *)
    TTA.SetFUAsEmpty(ind0)

    (* Check for free *)
    Assert.IsTrue(TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true)))




(*
[<Test>]
let SimpleAdd () =
    // 2 + 3
    let board = new Board [|(fun () -> new Cell([|0; 0|], 1, fun a -> a.[0] + a.[1])), 2|]
    board.Step [|Mvc (2, (0<ln>, 0<col>, 0<port>))|]
    board.Step [|Mvc (3, (0<ln>, 0<col>, 1<port>))|]
    let r = board.GetResult 0<ln> 0<col>
    Assert.AreEqual(5, r)

[<Test>]
let Add () =
    // 2 + 3 + 4 + 5
    let board = new Board [|(fun () -> new Cell([|0; 0|], 1, fun a -> a.[0] + a.[1])), 2|]
    board.Step [|Mvc (2, (0<ln>, 0<col>, 0<port>)); Mvc (4, (0<ln>, 1<col>, 0<port>))|]
    board.Step [|Mvc (3, (0<ln>, 0<col>, 1<port>)); Mvc (5, (0<ln>, 1<col>, 1<port>))|]
    board.Step [|Mov ((0<ln>, 1<col>), (0<ln>, 0<col>, 0<port>))|]
    board.Step [|Mov ((0<ln>, 0<col>), (0<ln>, 0<col>, 1<port>))|]
    let r = board.GetResult 0<ln> 0<col>
    Assert.AreEqual(14, r)

[<Test>]
let AddMult () =
    // 2 + 3 + 4 * 5
    let board = 
        new Board
            [|
                (fun () -> new Cell([|0; 0|], 1, fun a -> a.[0] + a.[1])), 2
                (fun () -> new Cell([|0; 0|], 1, fun a -> a.[0] * a.[1])), 2
            |]
    board.Step [|Mvc (2, (0<ln>, 0<col>, 0<port>)); Mvc (4, (1<ln>, 0<col>, 0<port>))|]
    board.Step [|Mvc (3, (0<ln>, 0<col>, 1<port>)); Mvc (5, (1<ln>, 0<col>, 1<port>))|]
    board.Step [|Mov ((1<ln>, 0<col>), (0<ln>, 0<col>, 0<port>))|]
    board.Step [|Mov ((0<ln>, 0<col>), (0<ln>, 0<col>, 1<port>))|]
    let r = board.GetResult 0<ln> 0<col>
    Assert.AreEqual(25, r)

*)