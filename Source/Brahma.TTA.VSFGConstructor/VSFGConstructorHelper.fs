module Brahma.TTA.VSFGConstructorHelper

open System
open System.IO

open Microsoft.FSharp.Compiler.SourceCodeServices

 let parseAndCheckSingleFile (input) (checker: FSharpChecker)  = 
        let file = Path.ChangeExtension(System.IO.Path.GetTempFileName(), "fsx")  
        File.WriteAllText(file, input)
        let projOptions = 
            checker.GetProjectOptionsFromScript(file, input)
           |> Async.RunSynchronously

        checker.ParseAndCheckProject(projOptions) 
        |> Async.RunSynchronously
    

type TypedTreeGetter(_input: string) = 
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let checkProjectResults = parseAndCheckSingleFile(_input) checker
    let checkedFile = checkProjectResults.AssemblyContents.ImplementationFiles.[0]
    let Decls = 
        match checkedFile.Declarations.[0] with 
        | FSharpImplementationFileDeclaration.Entity (e, subDecls) -> subDecls
        | _ -> failwith "unexpected"

    new(_input) = TypedTreeGetter(_input)
   
    member this.CountOfDecl = Decls.Length

    member this.getFSharpExpr (ind: int) =  
        match Decls.[ind] with 
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(fs, args, expr)  -> expr
            | _ -> failwith "unexpected"

    member this.getFSharpFunctionalSymbol (ind: int) =  
       match Decls.[ind] with 
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(fs, args, expr) -> fs
            | _ -> failwith "unexpected"

    member this.getFSharpArgs (ind: int) =  
       match Decls.[ind] with 
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(fs, args, expr)  -> args
            | _ -> failwith "unexpected"


