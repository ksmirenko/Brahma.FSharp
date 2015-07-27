namespace Brahma.TTA.VirtualTTA

[<Measure>] type ln
[<Measure>] type col
[<Measure>] type port

type OperationType = ADD_TYPE | SUB_TYPE | DIV_TYPE | REGISTER_TYPE | EQ_TYPE | GT_TYPE | LT_TYPE | GEG_TYPE | LEQ_TYPE | MULTIPLEXOR_TYPE

(**
 * bool value = true, if FU is free
 *            = false, if FU isn't free
 * string values -- ports' names
 * Ex:
 * ADD("in1", "in2", "out", true)
 *)
type FunctionUnitType = 
    | ADD of string * string * string * bool
    | SUB of string * string * string * bool
    | DIV of string * string * string * bool
    | REGISTER of string * bool

module FunctionUnit = 
    (**
     * TODO: Subsequently, the code should be parameterized
     * This is just a trial version
     *)
    let toString (fu : FunctionUnitType) (c : int<col>) (p : int<port>) = 
        match fu with
        | ADD(port0, port1, port2, _) -> "ADD" + c.ToString() + "." + (if p = 0<port> then port0 elif p = 1<port> then port1 elif p = 2<port> then port2 else "-1")
        | SUB(port0, port1, port2, _) -> "SUB" + c.ToString() + "." + (if p = 0<port> then port0 elif p = 1<port> then port1 elif p = 2<port> then port2 else "-1")
        | DIV(port0, port1, port2, _) -> "DIV" + c.ToString() + "." + (if p = 0<port> then port0 elif p = 1<port> then port1 elif p = 2<port> then port2 else "-1")
        | REGISTER(port0, _) -> "RF" + c.ToString() + "." + (if p = 0<port> then port0 else "-1")

    let isFree fu = 
        match fu with
        | ADD(_, _, _, b) -> b = true
        | SUB(_, _, _, b) -> b = true
        | DIV(_, _, _, b) -> b = true
        | REGISTER(_, b) -> b = true

    let setAsFree fu = 
        match fu with
        | ADD(x, y, z, _) -> ADD(x, y, z, true)
        | SUB(x, y, z, _) -> SUB(x, y, z, true)
        | DIV(x, y, z, _) -> DIV(x, y, z, true)
        | REGISTER(x, _) -> REGISTER(x, true)

    let setAsNonFree fu = 
        match fu with
        | ADD(x, y, z, _) -> ADD(x, y, z, false)
        | SUB(x, y, z, _) -> SUB(x, y, z, false)
        | DIV(x, y, z, _) -> DIV(x, y, z, false)
        | REGISTER(x, _) -> REGISTER(x, false)


type TTA(FUs : array<FunctionUnitType * int>, countOfBuses : int) = 
    let board = Array.init FUs.Length (fun i -> Array.init (snd FUs.[i]) (fun _ -> (fst FUs.[i])))

    (**
     * buses.[i] = true, if it is free
     *           = false, if it isn't free
     *)
    let buses = Array.init countOfBuses (fun _ -> true)

    member this.BusCount() = 
        buses.Length

    member this.IsFreeBus() = 
        Array.exists (fun x -> x = true) buses

    member this.GetFU(i : int<ln>, j : int<col>) =
        board.[int i].[int j] 
        
    (**
     * It is required to have at least one free bus
     * Ex:
     * let tta = new TTA(..)
     * ...
     * if (tta.IsFreeBus())
     * then tta.TakeABus()
     *)
    member this.TakeABus() =
        buses.[(Array.findIndex (fun x -> x = true) buses)] <- false

    member this.ReleaseAllBuses() = 
        for i in [0..buses.Length - 1] do
            buses.[i] <- true

    member this.SetFUAsFree (i : int<ln>, j : int<col>) =
        board.[int i].[int j] <- (FunctionUnit.setAsFree board.[int i].[int j])

    member this.SetFUAsNonFree (i : int<ln>, j : int<col>) = 
        board.[int i].[int j] <- (FunctionUnit.setAsNonFree board.[int i].[int j])

    (**
     * It is required to have at least one free function unit
     *)
    member this.GetFreeFU (opType : OperationType) = 
        let resultIndex = ref (-1<ln>, -1<col>)

        for i in [0..board.Length - 1] do
            let fuWithSameType = board.[i]

            (**
             * Check type of elems from array @fuWithSameTypes
             * If types from @fuWithSametypes and @opType are same, return index of first free function unit
             *)
            let elem = 
                match fuWithSameType.[0] with
                    | ADD(_, _, _, _) -> match opType with
                                         | ADD_TYPE -> Some(Array.findIndex FunctionUnit.isFree fuWithSameType)
                                         | _ -> None
                    | SUB(_, _, _, _) -> match opType with
                                         | SUB_TYPE -> Some(Array.findIndex FunctionUnit.isFree fuWithSameType)
                                         | _ -> None
                    | DIV(_, _, _, _) -> match opType with
                                         | DIV_TYPE -> Some(Array.findIndex FunctionUnit.isFree fuWithSameType)
                                         | _ -> None
                    | REGISTER(_, _) -> match opType with
                                         | REGISTER_TYPE -> Some(Array.findIndex FunctionUnit.isFree fuWithSameType)
                                         | _ -> None
            match elem with
                | Some(column) ->
                    resultIndex := (i * 1<ln>, column * 1<col>) 
                | None -> 
                    ()

        !resultIndex

    member this.IsFreeFU (opType : OperationType) = 
        let isHaveFreeFU = ref false

        for fuWithSameType in board do
            let isEmpty = 
                match fuWithSameType.[0] with
                    | ADD(_, _, _, _) -> match opType with
                                         | ADD_TYPE -> Some(Array.isEmpty (Array.filter FunctionUnit.isFree fuWithSameType))
                                         | _ -> None
                    | SUB(_, _, _, _) -> match opType with
                                         | SUB_TYPE -> Some(Array.isEmpty (Array.filter FunctionUnit.isFree fuWithSameType))
                                         | _ -> None
                    | DIV(_, _, _, _) -> match opType with
                                         | DIV_TYPE -> Some(Array.isEmpty (Array.filter FunctionUnit.isFree fuWithSameType))
                                         | _ -> None
                    | REGISTER(_, _) -> match opType with
                                         | REGISTER_TYPE -> Some(Array.isEmpty (Array.filter FunctionUnit.isFree fuWithSameType))
                                         | _ -> None

            match isEmpty with
                | Some(x) -> if x = false then isHaveFreeFU := true
                | None -> ()

        !isHaveFreeFU