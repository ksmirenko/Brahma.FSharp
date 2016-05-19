module CYK.Tests
open AlgorithmCYK
open NUnit.Framework

[<Test>]
let ``CYK returns "The string can be generated. " (grammar 1)`` () =
  let result = CYK [|("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")|] "baaba" "S"
  printfn "%s" result
  Assert.AreEqual("The string can be generated. ", result)

[<Test>]
let ``CYK returns "The string cannot be generated. " (grammar 1)`` () =
  let result = CYK [|("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")|] "baaababbbabab" "S"
  printfn "%s" result
  Assert.AreEqual("The string cannot be generated. ", result)

[<Test>]
let ``CYK returns "The string's lenght < 1. " (grammar 1)`` () =
  let result = CYK [|("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")|] "" "S"
  printfn "%s" result
  Assert.AreEqual("The string's lenght < 1. ", result)

[<Test>]
let ``CYK returns "The string can be generated. " (grammar 2)`` () =
  let result = CYK [|("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")|] "bccbcabcb" "S"
  printfn "%s" result
  Assert.AreEqual("The string can be generated. ", result)

[<Test>]
let ``CYK returns "The string cannot be generated. " (grammar 2)`` () =
  let result = CYK [|("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")|] "cccccaaaabbb" "S"
  printfn "%s" result
  Assert.AreEqual("The string cannot be generated. ", result)

[<Test>]
let ``CYK returns "The string's lenght < 1. " (grammar 2)`` () =
  let result = CYK [|("S","AB"); ("A","CC"); ("B","BC"); ("C","CB"); ("C","BA"); ("B","b"); ("A","a"); ("A","c"); ("C","c")|] "" "S"
  printfn "%s" result
  Assert.AreEqual("The string's lenght < 1. ", result)