module AlgorithmCYK.ForGraphs.Tests

open AlgorithmCYK.ForGraphs
open NUnit.Framework

[<TestFixture>]

type TestsGrammar1() = 
    
    let (rl1, start1) = ([|("S","AB"); ("S","BC"); ("A","BA"); ("B","CC"); ("C","AB"); ("B","b"); ("A","a"); ("C","a")|], "S")
    let nonterm = [|"A";"B";"C";"S"|]

    [<Test>]
    member this.``CYKsearch in empty matrix`` () =
      let mtrx = [|[|"";""|];[|"";""|]|]
      let result = search mtrx 2 5 rl1 nonterm start1
      Assert.AreEqual([||], result)

    [<Test>]
    member this.``CYKsearch in graph1`` () =
      let mtrx = [|[|"";"ababab";"";"";"";""|];[|"";"";"ababbab";"";"";""|];[|"";"";"";"abbababa";"";""|];[|"";"";"";"abaabab";"";""|];[|"";"";"";"";"ababab";""|];[|"";"";"";"";"";"bbbbb"|]|]
      let result = search mtrx 16 17 rl1 nonterm start1
      Assert.AreEqual([|"abababababbababba";"bababababbababbab";"ababababbababbaba";"babababbababbabab"|], result)

    [<Test>]
    member this.``CYKsearch in graph2`` () =
      let mtrx = [|[|"";"aba"|];[|"";""|]|]
      let result = search mtrx 4 6 rl1 nonterm start1
      Assert.AreEqual([||], result)

