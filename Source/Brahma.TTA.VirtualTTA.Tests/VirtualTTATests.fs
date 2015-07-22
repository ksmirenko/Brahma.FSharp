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

    TTA.SetFUAsNonFree(ind)
    let isFreeFU = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsFalse(isFreeFU)

    TTA.SetFUAsFree(ind)
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
    TTA.SetFUAsNonFree(ind0)

    (* Check second FU *)
    let isFreeFU1 = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsTrue(isFreeFU1)
    let ind1 = TTA.GetFreeFU(ADD(0<inPort>, 0<inPort>, true))
    TTA.SetFUAsNonFree(ind1)

    (* All FUs are not free *)
    let isAnybodyFree = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsFalse(isAnybodyFree)

    (* Free one FU *)
    TTA.SetFUAsFree(ind0)

    (* Check for free *)
    Assert.IsTrue(TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true)))

[<Test>]
let twoDistinctFU() = 
    let FU0 = ADD(0<inPort>, 0<inPort>, true)
    let FU1 = SUB(0<inPort>, 0<inPort>, true)
    let TTA = new TTA([| (FU0, 1); (FU1, 1) |], 1)

    (* Check first FU *)
    let isFreeFU0 = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsTrue(isFreeFU0)
    let ind0 = TTA.GetFreeFU(ADD(0<inPort>, 0<inPort>, true))
    TTA.SetFUAsNonFree(ind0)

    (* Check second FU *)
    let isFreeFU1 = TTA.IsFreeFU(SUB(0<inPort>, 0<inPort>, true))
    Assert.IsTrue(isFreeFU1)
    let ind1 = TTA.GetFreeFU(SUB(0<inPort>, 0<inPort>, true))
    TTA.SetFUAsNonFree(ind1)

    (* All FUs are not free *)
    let isAnybodyFree0 = TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true))
    Assert.IsFalse(isAnybodyFree0)
    let isAnybodyFree1 = TTA.IsFreeFU(SUB(0<inPort>, 0<inPort>, true))
    Assert.IsFalse(isAnybodyFree1)

    (* Free one FU *)
    TTA.SetFUAsFree(ind0)

    (* Check for free *)
    Assert.IsTrue(TTA.IsFreeFU(ADD(0<inPort>, 0<inPort>, true)))
    Assert.IsFalse(TTA.IsFreeFU(SUB(0<inPort>, 0<inPort>, true)))

    TTA.SetFUAsFree(ind1)
    Assert.IsTrue(TTA.IsFreeFU(SUB(0<inPort>, 0<inPort>, true)))


[<Test>]
let buses() = 
    let FU = ADD(0<inPort>, 0<inPort>, true)
    let TTA = new TTA([| (FU, 2) |], 3)

    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsFalse(TTA.isFreeBus())

    TTA.ReleaseAllBuses()

    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsFalse(TTA.isFreeBus())

    TTA.ReleaseAllBuses()

    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()

    TTA.ReleaseAllBuses()

    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.isFreeBus())
    TTA.TakeABus()
    Assert.IsFalse(TTA.isFreeBus())