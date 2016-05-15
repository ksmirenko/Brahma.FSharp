module ArrayFunс.Tests
open NUnit.Framework

[<Test>]
let ``Map test``() =
  let arr = [|for i in 1..1000000 -> i|]
  let t = ArrayFunc.init arr
  let outarr = ArrayFunc.getResult (ArrayFunc.arrayMap (<@ fun x -> x + 2 @>) arr t) t
  Assert.AreEqual ([|for i in 1..1000000 -> i + 2|], outarr)

[<Test>]
let ``Mapi test``() =
  let arr = [|for i in 1..10 -> i|]
  let t = ArrayFunc.init arr
  let outarr = ArrayFunc.getResult (ArrayFunc.arrayMapi (<@ fun x i -> x + i @>) arr t) t
  Assert.AreEqual ([|1; 3; 5; 7; 9; 11; 13; 15; 17; 19|], outarr)

[<Test>]
let ``Map2 test``() =
  let arr1 = [|for i in 1..10 -> i|]
  let arr2 = [|for i in 1..10 -> i|]
  let t = ArrayFunc.init arr1
  let outarr =  ArrayFunc.getResult (ArrayFunc.arrayMap2 (<@ fun x i -> x + i @>) arr1 arr2 t) t
  Assert.AreEqual ([|for i in 1..10 -> 2 * i|], outarr)

[<Test>]
let ``Reverse test``() =
  let arr = [|for i in 1..10 -> i|]
  let t = ArrayFunc.init arr
  let outarr = ArrayFunc.getResult (ArrayFunc.arrayReverse arr t) t
  Assert.AreEqual ([|10; 9; 8; 7; 6; 5; 4; 3; 2; 1|], outarr)


[<Test>]
let ``Reverse (Map) test``() =
  let arr = [|for i in 1..10 -> i|]
  let t = ArrayFunc.init arr
  let outarr = ArrayFunc.getResult (ArrayFunc.arrayReverse (ArrayFunc.arrayMap (<@ fun x -> x + 2 @>) arr t) t) t
  Assert.AreEqual ([|12; 11; 10; 9; 8; 7; 6; 5; 4; 3|], outarr)