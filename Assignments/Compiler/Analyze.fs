/// Analyzes a programs variables, noting whether or not the variable is closed on or is a local
module Analyze
open Ast'
open Utils
open State

/// Represents the Type of a variable: Local or ClosedOn
/// Parameter and Function are added for additional context awareness, should it be needed at somepoint...
type VariableType =
    | Local
    | ClosedOn
    | Function of VariableType
    | Parameter of VariableType
    member this.IsClosed = 
        match this with
        | ClosedOn -> true
        | _ -> false

module VarScope = 
    let add name ty (map:VariableType ref Scope) = 
        map.Add name (ref ty)

    let inLocalScope name (map:VariableType ref Scope) =
        map.InLocalScope name
    
    let closeOn name (scope:VariableType ref Scope) = 
        let i = scope.[name]
        i := match !i with
             | Function _ -> Function ClosedOn
             | Parameter _ -> Parameter ClosedOn
             | _ -> ClosedOn

    let push (map:VariableType ref Scope) =
        map.Push()
    
    let finish (map:VariableType ref Scope) =
        map.Finish()
    
    let lookup name (map:VariableType ref Scope) =
        map.[name]
    
    let empty : VariableType ref Scope =
        SMap.empty

/// A Builder Type for analyzing a program
/// Inherits StateMonad
type VariableAnalyzer() =
    inherit StateMonad()
    member this.addLocal     name scope = VarScope.add name Local scope, ()
    member this.addFunction  name scope = VarScope.add name (Function Local) scope, ()
    member this.addParameter name scope = VarScope.add name (Parameter Local) scope, ()
    member this.scope             scope = this.GetState scope
    member this.enterNewScope     scope = VarScope.push scope, ()
    member this.leaveScope        scope = VarScope.finish scope, ()
    member this.closeOn name      scope =
        if not <| VarScope.inLocalScope name scope then
            VarScope.closeOn name scope
        (scope,())

let analyzer = VariableAnalyzer()

let rec analyze ast =
    analyzer {
        match ast with 
        | IntConst _          | FloatConst _
        | BoolConst _         | StringConst _
        | PrimitiveFunction _ | Expression.Closure _
        | Scope(_,_,_) -> // encountering a scope is not possible since we build them in this function
            return ast
        | Return ret ->
            let! ret = analyze ret
            return Return(ret)
        | Binding(name,value,body) ->
            let! f = analyzer.addLocal name
            let! value = analyze value
            let! body = analyze body
            return Expression.Binding(name,value,body)
        | VariableRef name ->
            if name <> "this" then
                do! analyzer.closeOn name
            else return ()
            return ast
        | If(tst,thn,els) ->
            let! tst = analyze tst
            let! thn = analyze thn
            let! els = analyze els 
            return If(tst,thn,els)
        | FunctionApplication(func,args) -> 
            let! func = analyze func
            let! args = analyzer.map analyze args
            return FunctionApplication(func,args) 
        | FunctionBindings(funcs,body) ->
            let (names,fs) = List.unzip funcs
            do! analyzer.iter analyzer.addFunction names
            let! fse = analyzer.map analyze fs
            let! body = analyze body 
            return FunctionBindings(List.zip names fse,body)
        | Func(parameters,body) ->
            do! analyzer.enterNewScope
            do! analyzer.iter analyzer.addParameter parameters
            let! body = analyze body
            let! functionScope = analyzer.scope
            do! analyzer.leaveScope
            return Scope(Func(parameters,body),functionScope,0)
        | While(tst,body) ->
            let! tst = analyze tst
            do! analyzer.enterNewScope
            let! body = analyze body
            let! scope = analyzer.scope
            do! analyzer.leaveScope
            return While(tst,Scope(body,scope,0))
        | Mutate(name,exp) ->
            do! analyzer.closeOn name
            let! exp = analyze exp
            return Mutate(name,exp)
        | MemberAccess(o,m) ->
            let! o = analyze o
            return MemberAccess(o,m)
        | MemberBind(o,m,v) -> 
            let! o = analyze o
            let! v = analyze v
            return MemberBind(o,m,v) 
        | MethodCall(o,m,args) ->
            let! o = analyze o
            let! args = analyzer.map analyze args
            return MethodCall(o,m,args)
        | New(ctor,args) -> 
            let! ctor = analyze ctor
            let! args = analyzer.map analyze args
            return New(ctor,args)
        | Sequence s -> 
            let! s = analyzer.map analyze s
            return Sequence s
    }