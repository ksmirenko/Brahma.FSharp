// Copyright (c) 2017 Kirill Smirenko <k.smirenko@gmail.com>
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

type DeclSpecifierPack<'lang> (?funQual:FunQualifier<'lang>,
                               ?addrSpaceQual:AddressSpaceQualifier<'lang>,
                               ?accessQual:AccessQualifier<'lang>,
                               ?storClassSpec:StorageClassSpecifier<'lang>,
                               ?typeSpec:Type<'lang>,
                               ?typeQuals:TypeQualifier<'lang> list) =
    inherit Node<'lang>()

    let mutable _funQual = funQual
    let mutable _addrSpaceQual =
        match addrSpaceQual with
        | Some x -> x
        | None -> Default
    let mutable _accessQual = accessQual
    let mutable _storClassSpec = storClassSpec
    let mutable _typeSpec = typeSpec
    let mutable _typeQuals =
        match typeQuals with
        | Some x -> x
        | None -> []

    override this.Children = []
    member this.FunQual
        with get() = _funQual
        and set v = _funQual <- v
    member this.AddressSpaceQual
        with get() = _addrSpaceQual
        and set v = _addrSpaceQual <- v
    member this.AccessQual
        with get() = _accessQual
        and set v = _accessQual <- v
    member this.StorageClassSpec
        with get() = _storClassSpec
        and set v = _storClassSpec <- v
    member this.Type
        with get() = _typeSpec
        and set v = _typeSpec <- v
    member this.TypeQuals
        with get() = _typeQuals
        and set v = _typeQuals <- v

    member this.AddTypeQual tq =
        _typeQuals <- tq :: _typeQuals

    override this.Equals(other) =
        match other with
        | :? DeclSpecifierPack<'lang> as o ->
            this.FunQual = o.FunQual
            && this.AddressSpaceQual = o.AddressSpaceQual
            && this.AccessQual = o.AccessQual
            && this.StorageClassSpec = o.StorageClassSpec
            && this.Type = o.Type
            && this.TypeQuals = o.TypeQuals
        | _ -> false
