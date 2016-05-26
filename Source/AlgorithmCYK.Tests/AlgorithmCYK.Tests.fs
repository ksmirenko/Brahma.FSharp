module CYK.Tests
open AlgorithmCYK
open NUnit.Framework

[<Test>]
let ``CYK returns true (grammar 1)`` () =
  let result = CYK [("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")] [|"S";"A";"B";"C"|] "baaba" "S"
  Assert.AreEqual(true, result)

[<Test>]
let ``CYK returns false (grammar 1)`` () =
  let result = CYK [("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")] [|"S";"A";"B";"C"|] "baaababbbabab" "S"
  Assert.AreEqual(false, result)

[<Test>]
let ``CYK returns true (empty string) (grammar 1)`` () =
  let result = CYK [("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")] [|"S";"A";"B";"C"|] "" "S"
  Assert.AreEqual(true, result)

[<Test>]
let ``CYK returns true (grammar 2)`` () =
  let result = CYK [("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")] [|"S";"A";"B";"C"|] "bccbcabcb" "S"
  Assert.AreEqual(true, result)

[<Test>]
let ``CYK returns false (grammar 2)`` () =
  let result = CYK [("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")] [|"S";"A";"B";"C"|] "cccccaaaabbb" "S"
  Assert.AreEqual(false, result)

[<Test>]
let ``CYK returns true(empty string) (grammar 2)`` () =
  let result = CYK [("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")] [|"S";"A";"B";"C"|] "" "S"
  Assert.AreEqual(true, result)