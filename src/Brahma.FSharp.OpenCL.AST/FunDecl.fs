// Copyright (c) 2012, 2013 Semyon Grigorev <rsdpisuy@gmail.com>
// All rights reserved.
// 
// The contents of this file are made available under the terms of the
// Eclipse Public License v1.0 (the "License") which accompanies this
// distribution, and is available at the following URL:
// http://www.opensource.org/licenses/eclipse-1.0.php
// 
// Software distributed under the License is distributed on an "AS IS" basis,
// WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
// the specific language governing rights and limitations under the License.
// 
// By using this software in any fashion, you are agreeing to be bound by the
// terms of the License.

namespace Brahma.FSharp.OpenCL.AST

// TODO: change in Brahma.FSharp
type FunFormalArg<'lang>(declSpecs:DeclSpecifierPack<'lang>, name:string) =
    inherit Statement<'lang>()
    override this.Children = []
    member this.DeclSpecs = declSpecs
    member this.Name = name

    override this.Equals(other) =
        match other with
        | :? FunFormalArg<'lang> as o ->
            this.DeclSpecs.Equals(o.DeclSpecs)
            && this.Name.Equals(o.Name)
        | _ -> false

// TODO: change in Brahma.FSharp
type FunDecl<'lang>(declSpecs:DeclSpecifierPack<'lang>, name:string, args:List<FunFormalArg<'lang>>, body:Statement<'lang>) =
    inherit TopDef<'lang>()
    override this.Children = []
    member this.DeclSpecs = declSpecs
    member this.Name = name
    member this.Args = args
    member this.Body = body

    override this.Equals(other) =
        match other with
        | :? FunDecl<'lang> as o ->
            this.DeclSpecs.Equals(o.DeclSpecs)
            && this.Name.Equals(o.Name)
            && this.Args.Equals(o.Args)
            // NB: body is omitted in this check
        | _ -> false

