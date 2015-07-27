module Brahma.TTA.VirtualTTA.Tests

open NUnit.Framework

[<Test>]
let oneFU () = 
    let FU = ADD("in1", "in2", "out", true)
    let TTA = new TTA([| (FU, 1) |], 1)
    
    (* Information about function unit doesn't matter, matters just type of FU (ex: ADD, SUB, etc..) 
     * So, we can write TTA.IsFreeFU(ADD(13<inPort>, 16<inPort>, false))
     *)
    let isFreeFU = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsTrue(isFreeFU)

    let ind = TTA.GetFreeFU(ADD_TYPE)
    Assert.IsTrue((ind = (0<ln>,0<col>)))

    TTA.SetFUAsNonFree(ind)
    let isFreeFU = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsFalse(isFreeFU)

    TTA.SetFUAsFree(ind)
    let isNonFreeFU = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsTrue(isNonFreeFU)

[<Test>]
let twoSameFU() = 
    let FU = ADD("in1", "in2", "out", true)
    let TTA = new TTA([| (FU, 2) |], 1)

    (* Check first FU *)
    let isFreeFU0 = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsTrue(isFreeFU0)
    let ind0 = TTA.GetFreeFU(ADD_TYPE)
    TTA.SetFUAsNonFree(ind0)

    (* Check second FU *)
    let isFreeFU1 = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsTrue(isFreeFU1)
    let ind1 = TTA.GetFreeFU(ADD_TYPE)
    TTA.SetFUAsNonFree(ind1)

    (* All FUs are not free *)
    let isAnybodyFree = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsFalse(isAnybodyFree)

    (* Free one FU *)
    TTA.SetFUAsFree(ind0)

    (* Check for free *)
    Assert.IsTrue(TTA.IsFreeFU(ADD_TYPE))

[<Test>]
let twoDistinctFU() = 
    let FU0 = ADD("in1", "in2", "out", true)
    let FU1 = SUB("in1", "in2", "out", true)
    let TTA = new TTA([| (FU0, 1); (FU1, 1) |], 1)

    (* Check first FU *)
    let isFreeFU0 = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsTrue(isFreeFU0)
    let ind0 = TTA.GetFreeFU(ADD_TYPE)
    TTA.SetFUAsNonFree(ind0)

    (* Check second FU *)
    let isFreeFU1 = TTA.IsFreeFU(SUB_TYPE)
    Assert.IsTrue(isFreeFU1)
    let ind1 = TTA.GetFreeFU(SUB_TYPE)
    TTA.SetFUAsNonFree(ind1)

    (* All FUs are not free *)
    let isAnybodyFree0 = TTA.IsFreeFU(ADD_TYPE)
    Assert.IsFalse(isAnybodyFree0)
    let isAnybodyFree1 = TTA.IsFreeFU(SUB_TYPE)
    Assert.IsFalse(isAnybodyFree1)

    (* Free one FU *)
    TTA.SetFUAsFree(ind0)

    (* Check for free *)
    Assert.IsTrue(TTA.IsFreeFU(ADD_TYPE))
    Assert.IsFalse(TTA.IsFreeFU(SUB_TYPE))

    TTA.SetFUAsFree(ind1)
    Assert.IsTrue(TTA.IsFreeFU(SUB_TYPE))


[<Test>]
let buses() = 
    let FU = ADD("in1", "in2", "out", true)
    let TTA = new TTA([| (FU, 2) |], 3)

    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsFalse(TTA.IsFreeBus())

    TTA.ReleaseAllBuses()

    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsFalse(TTA.IsFreeBus())

    TTA.ReleaseAllBuses()

    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()

    TTA.ReleaseAllBuses()

    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsTrue(TTA.IsFreeBus())
    TTA.TakeABus()
    Assert.IsFalse(TTA.IsFreeBus())