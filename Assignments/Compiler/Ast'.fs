module Ast'

open Ast
open Utils

type 'a Scope = SMap<string,'a>

///<summary>
/// A Expression prime
/// This represents the possible different language forms, with the addtion of a Scope node and a function closure
///</summary>
type Expression'<'a> =
    | IntConst of  int
    | FloatConst of float
    | BoolConst of bool
    | StringConst of string
    | VariableRef of string
    | If of Expression'<'a> * Expression'<'a> * Expression'<'a> (*   test, then, else   *)
    | FunctionApplication of Expression'<'a> * Expression'<'a> list
    | Sequence of Expression'<'a> list
    | Binding of string * Expression'<'a> * Expression'<'a>
    | PrimitiveFunction of Primitive
    | Func of string list * Expression'<'a> (* params, body *)
    | FunctionBindings of (string * Expression'<'a>) list * Expression'<'a>
    | Return of Expression'<'a>
    | Mutate of string * Expression'<'a>
    | While of Expression'<'a> * Expression'<'a>
    | MemberAccess of Expression'<'a> * string
    | MemberBind of Expression'<'a> * string * Expression'<'a>
    | MethodCall of Expression'<'a> * string * Expression'<'a> list 
    | New of Expression'<'a> * Expression'<'a> list
    | Scope of Expression'<'a> * 'a Scope * int
    | Closure of int
    /// A conversion function from Expressions to Expression primes
    static member Convert ast = 
        match ast with
        | E.IntConst    i -> E'<'a>.IntConst    i
        | E.FloatConst  f -> E'.FloatConst  f
        | E.BoolConst   b -> E'.BoolConst   b
        | E.StringConst s -> E'.StringConst s
        | E.VariableRef v -> E'.VariableRef v
        | E.Return e -> E'.Return (E'.Convert e)
        | E.Mutate(var,e) -> E'.Mutate(var,E'.Convert e)
        | E.PrimitiveFunction prim -> E'.PrimitiveFunction prim
        | E.MemberAccess(o,m) -> E'.MemberAccess(E'.Convert o,m)
        | E.While(tst,e) -> E'.While(E'.Convert tst,E'.Convert e)
        | E.Func(parameters,body) -> E'.Func(parameters,E'.Convert body)
        | E.Binding(name,v,b) -> E'.Binding(name,E'.Convert v,E'.Convert b)
        | E.MemberBind(o,m,v) -> E'.MemberBind(E'.Convert o,m,E'.Convert v)
        | E.If(tst,thn,els) -> E'.If(E'.Convert tst,E'.Convert thn,E'.Convert els)
        | E.New(ctor,args) -> E'.New(E'.Convert ctor,args |> List.map E'.Convert )
        | E.MethodCall(o,m,args) -> E'.MethodCall(E'.Convert o,m,args |> List.map E'.Convert )
        | E.FunctionApplication(f,args) -> E'.FunctionApplication(E'.Convert f,List.map E'.Convert args)
        | E.FunctionBindings(funcs,body) -> E'.FunctionBindings(funcs |> List.map (fun (n,f) -> (n,E'.Convert f)) ,E'.Convert body)
        | E.Sequence s -> E'.Sequence <| List.map E'.Convert s
and private E'<'a> = Expression'<'a>
and private E = Ast.Expression
and Expression<'a> = Expression'<'a>

