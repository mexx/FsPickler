﻿module internal Nessos.FsPickler.FSharpPicklers

    open System
    open System.IO
    open System.Reflection
    open System.Threading
    open System.Runtime.Serialization

    open Microsoft.FSharp.Reflection

    open Nessos.FsPickler
    open Nessos.FsPickler.Reflection
    open Nessos.FsPickler.PicklerUtils

#if EMIT_IL
    open System.Reflection.Emit
    open Nessos.FsPickler.Emit
    open Nessos.FsPickler.PicklerEmit
#endif

    // F# union types

    type FsUnionPickler =

        static member Create<'Union> (resolver : IPicklerResolver) =
            // resolve tag reader methodInfo
            let tagReaderMethod =
                match FSharpValue.PreComputeUnionTagMemberInfo(typeof<'Union>, allMembers) with
                | null -> invalidOp "unexpected error"
                | :? PropertyInfo as p -> p.GetGetMethod(true)
                | :? MethodInfo as m -> m
                | _ -> invalidOp "unexpected error"

#if EMIT_IL
            let caseInfo =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.sortBy (fun uci -> uci.Tag)
                |> Array.map (fun uci ->
                    let fields = uci.GetFields()
                    let caseType = 
                        if fields.Length = 0 then None
                        else
                            match fields.[0].DeclaringType with
                            | dt when dt <> uci.DeclaringType -> Some dt
                            | _ -> None
                    let ctor = FSharpValue.PreComputeUnionConstructorInfo(uci, allMembers)
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    let tags = fields |> Array.map getTagFromMemberInfo
                    caseType, ctor, fields, tags, picklers)

            let picklerss = caseInfo |> Array.map (fun (_,_,_,_,picklers) -> picklers)

            let writerDele =
                DynamicMethod.compileAction3<Pickler [] [], WriteState, 'Union> "unionSerializer" (fun picklerss writer union ilGen ->
                    let tag = EnvItem<int>(ilGen)
                    let picklers = EnvItem<Pickler []>(ilGen)

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // read union tag & store to local value
                    union.Load ()
                    ilGen.EmitCall(OpCodes.Call, tagReaderMethod, null)
                    tag.Store ()

                    // select appropriate picklers & store
                    picklerss.Load ()
                    tag.Load ()
                    ilGen.Emit OpCodes.Ldelem_Ref
                    picklers.Store ()

                    // write tag to stream
                    writeInt writer "case" tag ilGen

                    // make jump table
                    tag.Load ()
                    ilGen.Emit(OpCodes.Switch, labels)

                    // emit cases
                    for i = 0 to caseInfo.Length - 1 do
                        let label = labels.[i]
                        let caseType,_,fields,tags,_ = caseInfo.[i]

                        ilGen.MarkLabel label
                        emitSerializeProperties fields tags writer picklers union ilGen
                        ilGen.Emit OpCodes.Ret
                )

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [] [], ReadState, 'Union> "unionDeserializer" (fun picklerss reader ilGen ->

                    let tag = EnvItem<int>(ilGen)
                    let picklers = EnvItem<Pickler []>(ilGen)

                    let labels = Array.init caseInfo.Length (fun _ -> ilGen.DefineLabel())

                    // read tag from stream & store
                    readInt reader "case" ilGen
                    tag.Store ()

                    // select appropriate picklers & store
                    picklerss.Load ()
                    tag.Load ()
                    ilGen.Emit OpCodes.Ldelem_Ref
                    picklers.Store ()

                    // make jump table
                    tag.Load ()
                    ilGen.Emit(OpCodes.Switch, labels)

                    // emit cases
                    for i = 0 to caseInfo.Length - 1 do
                        let label = labels.[i]
                        let _,ctor,fields,tags,_ = caseInfo.[i]

                        let ctorParams = fields |> Array.map (fun f -> f.PropertyType)

                        ilGen.MarkLabel label
                        emitDeserializeAndConstruct (Choice1Of2 ctor) ctorParams tags reader picklers ilGen
                        ilGen.Emit OpCodes.Ret
                )

            let writer w tag u = writerDele.Invoke(picklerss, w, u)
            let reader r tag = readerDele.Invoke(picklerss, r)
#else
            let tagReader = Delegate.CreateDelegate<Func<'Union,int>> tagReaderMethod

            let caseInfo =
                FSharpType.GetUnionCases(typeof<'Union>, allMembers) 
                |> Array.map (fun uci ->
                    let ctor = FSharpValue.PreComputeUnionConstructor(uci, allMembers)
                    let reader = FSharpValue.PreComputeUnionReader(uci, allMembers)
                    let fields = uci.GetFields()
                    let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
                    let tags = fields |> Array.map getTagFromMemberInfo
                    ctor, reader, fields, tags, picklers)

            let writer (w : WriteState) (tag : string) (u : 'Union) =
                let tag = tagReader.Invoke u
                w.Formatter.WriteByte "case" (byte tag)
                let _,reader,fields,tags,picklers = caseInfo.[tag]
                let values = reader u
                for i = 0 to values.Length - 1 do
                    picklers.[i].UntypedWrite w tags.[i] (values.[i])

            let reader (r : ReadState) (tag : string) =
                let tag = int (r.Formatter.ReadByte "case")
                let ctor,_,fields,tags,picklers = caseInfo.[tag]
                let values = Array.zeroCreate<obj> picklers.Length
                for i = 0 to picklers.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead r tags.[i]

                ctor values |> fastUnbox<'Union>
#endif
            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = true)

    // F# record types

    type FsRecordPickler =
        
        static member Create<'Record>(resolver : IPicklerResolver) =

            let fields = FSharpType.GetRecordFields(typeof<'Record>, allMembers)
            let ctor = FSharpValue.PreComputeRecordConstructorInfo(typeof<'Record>, allMembers)

            let picklers = fields |> Array.map (fun f -> resolver.Resolve f.PropertyType)
            let tags = fields |> Array.map getTagFromMemberInfo

#if EMIT_IL
            let writer =
                if fields.Length = 0 then fun _ _ _ -> ()
                else
                    let writerDele =
                        DynamicMethod.compileAction3<Pickler [], WriteState, 'Record> "recordSerializer" (fun picklers writer record ilGen ->

                            emitSerializeProperties fields tags writer picklers record ilGen
                            
                            ilGen.Emit OpCodes.Ret)

                    fun w tag t -> writerDele.Invoke(picklers, w,t)

            let readerDele =
                DynamicMethod.compileFunc2<Pickler [], ReadState, 'Record> "recordDeserializer" (fun picklers reader ilGen ->

                    let ctorParams = fields |> Array.map (fun f -> f.PropertyType)

                    emitDeserializeAndConstruct (Choice2Of2 ctor) ctorParams tags reader picklers ilGen

                    ilGen.Emit OpCodes.Ret)
            
            let reader r tag = readerDele.Invoke(picklers, r)
#else
            let writer (w : WriteState) (tag : string) (r : 'Record) =
                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue r
                    picklers.[i].UntypedWrite w tags.[i] o
            
            let reader (r : ReadState) (tag : string) =
                let values = Array.zeroCreate<obj> fields.Length
                for i = 0 to fields.Length - 1 do
                    values.[i] <- picklers.[i].UntypedRead r tags.[i]

                ctor.Invoke values |> fastUnbox<'Record>
#endif

            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = false, useWithSubtypes = false)


    // F# exception types
    // Exception serialization is broken in F#; while types are ISerializable, added fields will not be serialized properly
    // Use a combination of ISerializable resolution and reflection to derive correct logic
    type FsExceptionPickler =
        
        static member Create<'Exception when 'Exception :> exn>(resolver : IPicklerResolver) =
            // the default ISerializable pickler that handles exception metadata serialization
            let defPickler = DotNetPicklers.ISerializablePickler.Create<'Exception>(resolver) :?> CompositePickler<'Exception>
            // separately serialize exception fields
            let fields = gatherFields typeof<'Exception> |> Array.filter(fun f -> f.DeclaringType = typeof<'Exception>)
            let fpicklers = fields |> Array.map (fun f -> resolver.Resolve f.FieldType)
            // extract tag names from properties, not fields
            let tags = FSharpType.GetExceptionFields(typeof<'Exception>, allMembers) |> Array.map (fun p -> p.Name)

#if EMIT_IL
            let writerDele = 
                if fields.Length = 0 then None
                else
                    DynamicMethod.compileAction3<Pickler [], WriteState, 'Exception> "exceptionSerializer" (fun picklers writer value ilGen ->

                        emitSerializeFields fields tags writer picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some

            let readerDele =
                if fields.Length = 0 then None
                else
                    DynamicMethod.compileAction3<Pickler [], ReadState, 'Exception> "exceptionDeserializer" (fun picklers reader value ilGen ->

                        emitDeserializeFields fields tags reader picklers value ilGen

                        ilGen.Emit OpCodes.Ret
                    ) |> Some


            let writer (w : WriteState) (tag : string) (e : 'Exception) =
                do defPickler.Writer w tag e
 
                match writerDele with
                | None -> ()
                | Some d -> d.Invoke(fpicklers, w, e)

            let reader (r : ReadState) (tag : string) =
                let e = defPickler.Reader r tag

                match readerDele with
                | None -> e
                | Some d -> d.Invoke(fpicklers, r, e) ; e
#else
            let writer (w : WriteState) (tag : string) (e : 'Exception) =
                do defPickler.Writer w tag e

                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = f.GetValue e
                    fpicklers.[i].UntypedWrite w tags.[i] o

            let reader (r : ReadState) (tag : string) =
                let e = defPickler.Reader r tag

                for i = 0 to fields.Length - 1 do
                    let f = fields.[i]
                    let o = fpicklers.[i].UntypedRead r tags.[i]
                    f.SetValue(e, o)
                e
#endif
            CompositePickler.Create(reader, writer, PicklerInfo.FSharpValue, cacheByRef = true, useWithSubtypes = true)
