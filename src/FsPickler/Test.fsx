﻿#I "../../bin/"
#r "FsPickler.dll"

open Nessos.FsPickler
open Nessos.FsPickler.Combinators

let qPickler = Pickler.auto<Quotations.Expr<int>>

let bp = Binary.pickle qPickler <@ 1 + 1 @>
let xp = Xml.pickle qPickler <@ 1 + 1 @>

Binary.unpickle qPickler bp
Xml.unpickle qPickler xp

let fPickler = Pickler.func<int, int>
let bp' = Binary.pickle fPickler (fun x -> x + 1)
let xp' = Xml.pickle fPickler (fun x -> x + 1)

Binary.unpickle fPickler bp' 41
Xml.unpickle fPickler xp' 41

let x = [| obj() |]
x.[0] <- box x

let p = Pickler.auto<obj>

Binary.pickle p x |> Binary.unpickle p
Xml.pickle p x |> Xml.unpickle p