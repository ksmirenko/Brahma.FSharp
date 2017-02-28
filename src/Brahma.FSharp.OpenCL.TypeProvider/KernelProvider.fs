module Brahma.FSharp.OpenCL.TypeProvider.KernelProvider

open Brahma.FSharp.OpenCL.TypeProvider.KernelReader
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Reflection

[<TypeProvider>]
type KernelProvider(config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let nspace = "Brahma.FSharp.OpenCL.TypeProvider.Provided"
    let assembly = Assembly.GetExecutingAssembly()

    let kernelProvider = ProvidedTypeDefinition(assembly, nspace, "KernelProvider", Some(typeof<obj>))

    let parameters = [ProvidedStaticParameter("PathToFile", typeof<string>)]

    do kernelProvider.DefineStaticParameters(parameters, fun typeName args ->
        let filePath = args.[0] :?> string
        let retProvider = ProvidedTypeDefinition(assembly,
                                                 nspace,
                                                 typeName,
                                                 Some typeof<obj>,
                                                 HideObjectMethods = true)
        readKernels filePath |> List.iter retProvider.AddMember
        retProvider
    )

    do
        this.AddNamespace(nspace, [kernelProvider])

[<assembly:TypeProviderAssembly>]
do ()
