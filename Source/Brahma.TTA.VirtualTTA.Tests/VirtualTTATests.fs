module Brahma.TTA.VirtualTTA.Tests

open NUnit.Framework

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