module internal Nessos.FsPickler.Zipper

    // Functional Zipper implementation

    type Tree<'T> =
        | Branch of 'T * Tree<'T> list

    type Context<'T> =
        | Top
        | Node of Tree<'T> list * 'T * Context<'T> * Tree<'T> list

    type Zipper<'T> = Context<'T> * Tree<'T>

    [<RequireQualifiedAccess>]
    module Zipper =

        let ofTree (t : Tree<'T>) : Zipper<'T> =  Top, t

        let up ((ctx,tree) : Zipper<'T>) : Zipper<'T> =
            match ctx with
            | Top -> invalidOp "cannot zip further up."
            | Node(left, t, ctx', right) -> ctx', Branch (t, left @ tree :: right)

        let down i ((ctx,tree) : Zipper<'T>) : Zipper<'T> =
            match tree with
            | Branch (_,[]) -> invalidOp "cannot unzip further down."
            | Branch (_,cs) when i >= cs.Length -> raise <| new System.IndexOutOfRangeException()
            | Branch (t,cs) ->
                let left = Seq.take i cs |> Seq.toList
                let c = cs.[i]
                let right = Seq.skip (i+1) cs |> Seq.toList
                let ctx' = Node(left, t, ctx, right)
                ctx', c


    type StackZipper<'T> () =

        let mutable stack = []
        let mutable context = Top

        member __.Push (t : 'T) =
            stack <- t :: stack
            context <- Node([], t, context, [])

        member __.Pop () =
            match stack with
            | [] -> invalidOp "Stack empty."
            | t :: tail ->
                match context with
                | Top -> failwith "impossible"
                | Node(_, _, Top, children) ->
                    let tr = Branch(t, children)
                    stack <- tail
                    context <- Top
                    t, tr

                | Node(_, _, Node(_, t', ctx, siblings), children) ->
                    let tr = Branch(t, children)
                    stack <- tail
                    context <- Node([], t', ctx, tr :: siblings)
                    t, tr

        member __.Stack = stack