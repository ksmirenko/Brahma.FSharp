namespace Brahma.TTA.VirtualTTA

[<Measure>] type ln
[<Measure>] type col
[<Measure>] type port
 
type Asm =
    | Mov of (int<ln> * int<col>) * (int<ln> * int<col> * int<port>)
    | Mvc of int * (int<ln> * int<col> * int<port>)

type Cell (ports : array<int>, triggerPortId, action) =
    let result = ref 0
    member this.Set v i = 
        ports.[i] <- v
        if i = triggerPortId
        then result := action ports
    member this.GetResult () = !result

type Board (rowDescriptions : array<(unit -> Cell) * int>) =
    let board = Array.init rowDescriptions.Length (fun i -> Array.init (snd rowDescriptions.[i]) (fun j -> (fst rowDescriptions.[i])()))
    member this.Step (instruction : array<Asm>) =
        instruction
        |> Array.iter
            (function | Mov ((lf, cf), (lt, ct, pt)) -> board.[int lt].[int ct].Set (board.[int lf].[int cf].GetResult()) (int pt)
                      | Mvc (v, (lt, ct, pt)) -> board.[int lt].[int ct].Set v (int pt))

    member this.GetResult (l : int<ln>) (c : int<col>) = board.[int l].[int c].GetResult()