module internal Nessos.FsPickler.PicklerResolution

    open System
    open System.Reflection
    open System.Collections.Generic

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.Zipper
    open Nessos.FsPickler.TypeShape
    open Nessos.FsPickler.PicklerFactory

    let isRecursivePickler (structure : Tree<Pickler>) =
        let root = structure.Value

        if typeof<MemberInfo>.IsAssignableFrom root.Type then false else

        let checkNode depth (p : Pickler) =
            if not p.Type.IsSealed || isISerializable p.Type then true
            else
                depth > 0 && isAssignableFrom root.Type p.Type

        Tree.exists checkNode structure

    let isOfFixedSize (structure : Tree<Pickler>) =
        if isRecursivePickler structure then false
        elif structure.Value.Type.IsArray then false
        else
            structure.Children |> List.forall (fun ch -> ch.Value.IsOfFixedSize)
            

//    /// resolves a suitable pickler implementation for given type
//    let generatePickler (resolver : IPicklerResolver) (factory : PicklerFactory) 
//                        (stack : PicklerStack) (t : Type) =
//
//        try
//            if stack.Types |> List.exists ((=) t) then
//                raise <| new PicklerGenerationException(t, "Detected invalid type recursion pattern.")
//
//            // step 1: resolve shape of given type
//            let shape = 
//                try TypeShape.resolve t
//                with UnSupportedShape -> raise <| NonSerializableTypeException(t)
//
//            // step 2: create an uninitialized pickler instance and register to the local cache
//            let p0 = UninitializedPickler.Create shape
//            stack.Push p0
//
//            // step 3: subtype pickler resolution
//            let result =
//                if t.BaseType <> null then
//                    match resolver.Resolve t.BaseType with
//                    | p when p.UseWithSubtypes -> Some p
//                    | _ -> None
//                else
//                    None
//
//            // step 4: generate pickler using the pickler factory
//            let pickler =
//                match result with
//                | Some r -> r
//                | None -> shape.Accept factory
//
//            let _, tree = stack.Pop()
//
//            // step 5: pickler generation complete, copy data to uninitialized binding and return it
//            p0.Unpack
//                {
//                    // should be IPicklerUnpacker<unit> but F# does not allow this
//                    new IPicklerUnpacker<bool> with
//                        member __.Apply<'T> (p : Pickler<'T>) =
//                            match p with
//                            | :? CompositePickler<'T> as p -> p.InitializeFrom pickler ; true
//                            | _ -> 
//                                let msg = sprintf "Unexpected pickler implementation '%O'" <| p.GetType()
//                                raise <| new PicklerGenerationException(p.Type, msg)
//                } |> ignore
//
//            Success p0
//
//        with 
//        // Store all NonSerializableTypeException's in cache
//        | :? NonSerializableTypeException as e when e.Type = t -> Exn.Error e
//        | :? NonSerializableTypeException as e ->
//            Exn.error <| NonSerializableTypeException(t, e.NonSerializableType)
//
//        // wrap/reraise everything else as PicklerGenerationExceptions
//        | :? PicklerGenerationException -> reraise ()
//        | e -> raise <| new PicklerGenerationException(t, inner = e) 


    /// recursively builds a pickler implementation for given type
    let generatePickler (globalCache : ICache<Type, Exn<Pickler>>) (t : Type) =

        // a temporary local cache is used to store early, unitialized instances
        // this keeps the global cache from being contaminated with partial state
        let stack = new StackZipper<Pickler> ()

        let rec resolver = 
            {
                new IPicklerResolver with
                    member __.IsSerializable t = (generate t).IsValue
                    member __.IsSerializable<'T> () = (generate typeof<'T>).IsValue

                    member __.Resolve t = (generate t).Value
                    member __.Resolve<'T> () = (generate typeof<'T>).Value :?> Pickler<'T>
            }

        and generate (t : Type) : Exn<Pickler> =
            match globalCache.Lookup t with
            | Some p -> p
            | None ->
                match stack.Stack |> List.tryFind (fun p -> p.Type = t) with
                | Some p -> 
                    stack.Push p 
                    let _ = stack.Pop () 
                    Success p

                | None ->
                    try
                        // step 2: generate pickler using the pickler factory
                        let shape = 
                            try TypeShape.resolve t
                            with UnSupportedShape -> raise <| NonSerializableTypeException(t)

                        let p0 = shape.Accept { new ITypeVisitor<Pickler> with member __.Visit<'T> () = new CompositePickler<'T> () :> Pickler }
                        do stack.Push p0

                        let generatedPickler = buildPickler resolver shape

                        let _,tree = stack.Pop ()

                        let isRec = isRecursivePickler tree
                        let isFixed = isOfFixedSize tree

                        p0.Unpack
                            {
                                // should be IPicklerUnpacker<unit> but F# does not allow this
                                new IPicklerUnpacker<bool> with
                                    member __.Apply<'T> (p : Pickler<'T>) =
                                        match p with
                                        | :? CompositePickler<'T> as p -> 
                                            p.InitializeFrom generatedPickler ; 
                                            p.SetIsOfFixedSize isFixed
                                            p.SetIsRecursiveType isRec
                                            true
                                        | _ -> 
                                            let msg = sprintf "Unexpected pickler implementation '%O'" <| p.GetType()
                                            raise <| new PicklerGenerationException(p.Type, msg)
                            } |> ignore

                        globalCache.Commit t <| Success p0

                    with 
                    // Store all NonSerializableTypeException's in cache
                    | :? NonSerializableTypeException as e when e.Type = t -> Exn.Error e
                    | :? NonSerializableTypeException as e ->
                        Exn.error <| NonSerializableTypeException(t, e.NonSerializableType)

                    // wrap/reraise everything else as PicklerGenerationExceptions
                    | :? PicklerGenerationException -> reraise ()
                    | e -> raise <| new PicklerGenerationException(t, inner = e) 


        generate t