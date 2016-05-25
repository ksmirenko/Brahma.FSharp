module CYK.Tests
open AlgorithmCYK
open NUnit.Framework

[<Test>]
let ``CYK returns "The string can be generated. " (grammar 1)`` () =
  let result = CYK [("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")] [|"S";"A";"B";"C"|] "baaba" "S"
  Assert.AreEqual(true, result)

[<Test>]
let ``CYK returns "The string cannot be generated. " (grammar 1)`` () =
  let result = CYK [("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")] [|"S";"A";"B";"C"|] "baaababbbabab" "S"
  Assert.AreEqual(false, result)

[<Test>]
let ``CYK returns "The string's lenght < 1. " (grammar 1)`` () =
  let result = CYK [("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")] [|"S";"A";"B";"C"|] "" "S"
  Assert.AreEqual(true, result)

[<Test>]
let ``CYK returns "The string can be generated. " (grammar 2)`` () =
  let result = CYK [("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")] [|"S";"A";"B";"C"|] "bccbcabcb" "S"
  Assert.AreEqual(true, result)

[<Test>]
let ``CYK returns "The string cannot be generated. " (grammar 2)`` () =
  let result = CYK [("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")] [|"S";"A";"B";"C"|] "cccccaaaabbb" "S"
  Assert.AreEqual(false, result)

[<Test>]
let ``CYK returns "The string's lenght < 1. " (grammar 2)`` () =
  let result = CYK [("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")] [|"S";"A";"B";"C"|] "" "S"
  Assert.AreEqual(true, result)