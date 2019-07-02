namespace State

///<summary>
/// A State monad builder type
///</summary>
type StateMonad() =
    member this.Bind(m,f) = 
        fun s -> 
            let (state,result) = m s
            f result state
    
    member this.ReturnFrom(m) = m
    member this.Return(x) =
        fun state -> (state,x)
    
    member this.Combine(x,y) =
        fun state ->
            let (state, res) = (x state)
            y state
    
    member this.Delay(f) =
        fun state ->  
            f () state

    member this.GetState state = (state,state)
    member this.SetState state _ = (state,())
    member this.UpdateState f s = (f s,())
    
    member this.map f l =
        (List.map f l, this.Return([]))
        ||> List.foldBack (fun p q -> 
            this.Bind(p,fun x ->
                this.Bind(q,fun y -> this.Return(x::y)))
        )

    member this.iter f l =
        (List.map f l, this.Return(()))
        ||> List.foldBack (fun p q -> 
            this.Bind(p,fun x -> q)
        )