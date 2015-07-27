module Tests

open NUnit.Framework
open System.IO
open System.Diagnostics

[<TestFixture>]
type AsmTests () =

    [<Test>]
    member this.``Test`` () =
        let proc = new Process ()
        proc.StartInfo.FileName <- "../../exec_asm_get_result.sh"
        proc.StartInfo.Arguments <- "../../one_alu_arch.adf ../../one_alu_asm.asm b 1 11"
        (* 
           Arguments:
             - path to .adf from script directory
             - path to asm from script directory
             - memory unit size: ‘b’ (MAU, a byte in byte-addressed memories), ‘h’ (double MAU), ‘w’ (quadruple
               word, a ‘word’ in byte-addressed 32-bit architectures), ‘g’ (giant words, 8 MAU’s)
             - number of units to return
             - start memory address
        *)
        proc.StartInfo.UseShellExecute <- false
        proc.Start() |> ignore
        proc.WaitForExit ()

        //data will be written to binary file
        let reader = new BinaryReader (File.Open ("../../output", FileMode.Open))
        let result = reader.ReadByte ()
        reader.Close () 

        Assert.IsTrue (15uy = result);