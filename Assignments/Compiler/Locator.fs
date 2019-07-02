module Locator
open Utils
open Ast'
open Analyze
open State

type VariableLocation =
    | Closure of int
    | Local of int
    override this.ToString() =
        match this with
        | Closure i -> i.ToString()
        | Local i -> i.ToString()

type FunctionInfo = string * string list * Expression<VariableLocation> * LocationInfo
and LocationInfo = {
        LocalCount     : int
        ClosureCount   : int
        VarScope       : VariableType ref Scope
        VarLocations   : VariableLocation Scope
        PrimitivesUsed : Set<Ast.Primitive>
        FloatTable     : Map<float,int>
        StringTable    : Map<string,int>
        FieldTable     : Map<string,int>
        Functions      : Map<int,FunctionInfo>
    }

module VarLocation = 
    let add name loc (locs:VariableLocation Scope) =
        locs.Add name loc
    let contains name (locs:VariableLocation Scope) =
        locs.Contains name

    let locate name state =
//        if contains name state.VarLocations then
//            state
//        else
            let res = VarScope.lookup name state.VarScope 
            match !res with
            | VariableType.Local
            | VariableType.Function VariableType.Local
            | VariableType.Parameter VariableType.Local ->
                { state with 
                    LocalCount = state.LocalCount + 1;
                    VarLocations = add name (Local state.LocalCount) state.VarLocations }
            | ClosedOn
            | Function ClosedOn
            | VariableType.Parameter ClosedOn ->
                let (frame,_) = state.VarScope.Lookup name
                { state with
                    ClosureCount = 1 + state.ClosureCount;
                    VarLocations = add name (Closure state.ClosureCount) state.VarLocations }
            | _ -> failwith "invalid var"

    let enter scope state =
        { state with 
            VarScope = scope;
            VarLocations = state.VarLocations.Push() }

    let empty vars = { 
            VarScope = vars;
            LocalCount = 0
            ClosureCount = 0
            VarLocations = SMap.empty<string,VariableLocation>
            FloatTable = Map.empty
            StringTable = Map.empty 
            FieldTable = Map.empty
            PrimitivesUsed = Set.empty
            Functions = Map.empty
    }

module Map =
    let addIfNotPresent key value map = 
        if Map.containsKey key map then
            map
        else Map.add key value map

type VariableLocator() =
    inherit StateMonad()
    
    member this.locate name  state = VarLocation.locate name state, ()
    member this.enter scope  state = VarLocation.enter scope state, ()
    member this.locations    state = state, state.VarLocations
    member this.currentScope state = state, state.VarScope
    member this.saveCounters state = state, (state.LocalCount, state.ClosureCount)

    member this.restoreScope scope state = 
        { state with
            VarScope = scope
            VarLocations = state.VarLocations.Finish() }, ()
    
    member this.restoreCounters (cnt,clo) state = 
        { state with LocalCount = cnt; ClosureCount = clo }, ()
    
    member this.rememberString str state = 
        { state with 
            StringTable = 
                Map.addIfNotPresent str state.StringTable.Count state.StringTable }, ()

    member this.rememberField str state = 
        { state with 
            FieldTable = 
                Map.addIfNotPresent str (if str = "constructor" then 0 else state.FieldTable.Count + 1) state.FieldTable }, ()

    member this.rememberFloat f state = 
        { state with 
            FloatTable = 
                Map.addIfNotPresent f state.FloatTable.Count state.FloatTable }, ()

    member this.notePrimitive prim state = 
        { state with 
            PrimitivesUsed = 
                Set.add prim state.PrimitivesUsed }, ()



let locator = VariableLocator()

let rec locate (ast:Expression<VariableType ref>) : LocationInfo -> LocationInfo * Expression<VariableLocation>  = 
    locator {
        match ast with
        | BoolConst b -> return BoolConst b 
        | IntConst  i -> return IntConst i
        | VariableRef v -> return VariableRef v
        | PrimitiveFunction prim  ->
            do! locator.notePrimitive prim
            return PrimitiveFunction prim 
        | StringConst s ->
            do! locator.rememberString s
            return StringConst(s)
        | FloatConst f ->
            do! locator.rememberFloat f
            return FloatConst f
        | Binding(name,v,body) ->
            do! locator.locate name
            let! value = locate v
            let! body  = locate body
            return Binding(name,value,body)
        | Scope(exp,scope,_) ->
            let! counters = locator.saveCounters
            let! currentScope = locator.currentScope
            do! locator.enter scope
            let! exp = locate exp
            let! locationScope = locator.locations
            let! (_,cloCount) = locator.saveCounters
            do! locator.restoreScope currentScope 
            do! locator.restoreCounters counters
            return Scope(exp,locationScope,cloCount)
        | While(tst,Scope(body,scope,_)) ->
            let! tst = locate tst

            let! (l,c) = locator.saveCounters
            do! locator.restoreCounters (l,0)
            let! currentScope = locator.currentScope

            do! locator.enter scope
            let! body = locate body
            let! locationScope = locator.locations
            let! (newL,cloCount) = locator.saveCounters
            locationScope.ClosureCount <- cloCount

            do! locator.restoreScope currentScope 
            do! locator.restoreCounters (newL,c)

            return While(tst,Scope(body,locationScope,cloCount))
        | FunctionBindings(funcs,body) -> 
            // Function bindings gets a little bit nasty because the name is needed when processing the function....
            // Yeah i want to change my ast now.... HAH!
            let processF (name,parameters,body) = locator {
                do! locator.iter locator.locate parameters
                let! body = locate body
                let! state = locator.GetState
                let cloId = state.Functions.Count
                let l = Map.add cloId (name,parameters,body,state) state.Functions
                do! locator.SetState { state with Functions = l}
                return (name,Expression.Closure(cloId))
            }
            // First make sure that the function names are given a coordinate
            // This is so that when the body is processed it can find them (ie mutual recursion)
            do! locator.iter (fst>>locator.locate) funcs
            let! funcs =
                funcs
                |> locator.map (function
                    | (name,Scope(Func(parameters,body),scope,_)) -> 
                        locator {
                            let! counters = locator.saveCounters
                            do! locator.restoreCounters (0,0)
                            let! currentScope = locator.currentScope
                            do! locator.enter scope
                            let! (n,closure) = processF (name,parameters,body)
                            let! locationScope = locator.locations
                            let! (_,cloCount) = locator.saveCounters
                            locationScope.ClosureCount <- cloCount
                            do! locator.restoreScope currentScope
                            do! locator.restoreCounters counters
                            return (n,Scope(closure,locationScope,cloCount))
                        }
                    | _ -> failwith "invalid function binding"
                )
            let! body = locate body
            return FunctionBindings(funcs,body)
        | Func(parameters,body) -> 
            do! locator.iter locator.locate parameters
            let! b = locate body
            return Func(parameters,b)
        | FunctionApplication(f,args) -> 
            let! f = locate f
            let! args = locator.map locate args
            return FunctionApplication(f,args)
        | Return ret -> 
            let! ret = locate ret
            return Return(ret)
        | Sequence s ->
            let! s = locator.map locate s
            return Sequence s
        | If(tst,thn,els) -> 
            let! tst = locate tst
            let! thn = locate thn
            let! els = locate els
            return If(tst,thn,els)
        | Mutate(name,v) -> 
            let! v = locate v
            return Mutate(name,v)
        | New(ctor,args) -> 
            let! ctor = locate ctor
            let! args = locator.map locate args
            return New(ctor,args)
        | MemberAccess(o,f) -> 
            let! o = locate o
            do! locator.rememberField f
            return MemberAccess(o,f)
        | MemberBind(o,f,v) -> 
            let! o = locate o
            do! locator.rememberField f
            let! v = locate v
            return MemberBind(o,f,v)
        | MethodCall(o,f,args) -> 
            let! o = locate o
            do! locator.rememberField f
            let! args = locator.map locate args
            return MethodCall(o,f,args)
        | Expression.Closure _ -> return failwith "Closure cant be located!"
        | Expression.While(_,_) -> return failwith "Unscoped While"
    }