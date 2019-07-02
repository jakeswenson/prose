module Helpers
open Ast

let binop func ffunc args = 
    match args with 
    | [Int lhs; Int rhs] -> func lhs rhs |> Int
    | [Float lhs; Float rhs] -> ffunc lhs rhs |> Float
    | [Float f; Int i] -> ffunc f (float(i)) |> Float
    | [Int i; Float f] -> ffunc (float(i)) f |> Float
    | _ -> failwith "Operands must be numeric"

let bincompop func ffunc args = 
    match args with 
    | [Int lhs; Int rhs] -> func lhs rhs |> Bool
    | [Float lhs; Float rhs] -> ffunc lhs rhs |> Bool
    | [Float f; Int i] -> ffunc f (float(i)) |> Bool
    | [Int i; Float f] -> ffunc (float(i)) f |> Bool
    | _ -> failwith "Operands must be numeric"

let eqop args = 
    match args with 
    | [Int lhs; Int rhs] -> lhs = rhs |> Bool
    | [Float lhs; Float rhs] -> lhs = rhs |> Bool
    | [Bool lhs; Bool rhs] -> lhs = rhs |> Bool
    | [Void as f; Void as v] -> f = v |> Bool
    | [Object(_,slots) ; Object(_,rhs)] -> obj.ReferenceEquals(!slots,!rhs) |> Bool
    | [ _ ; _ ] -> Bool false
    | _ -> failwith "== Expects two arguments"

let boolop func args = 
    match args with 
    | [ Bool b; Bool c] -> func b c |> Bool
    | _ -> failwith "Arguments should be two Booleans"

let boolunop func args = 
    match args with 
    | [ Bool b] -> func b |> Bool
    | _ -> failwith "Argument should be a Boolean"

let stringunop func args =
    match args with 
    | [ Object(Str s,slots) ] -> func s
    | _ -> failwith "Argument must be a string"

let print = 
    function 
    | [ Int i ] -> printf "%i" i
    | [ Float f ] -> printf "%f" f
    | [ Bool b ] -> printf "<%b>" b
    | [ Void ] -> printf "<void>"
    | [ Object(Str s,_) ] -> printf "%s" s
    | [ Object(Closure(_,_,_),_) ] -> printf "<closure>" 
    | [ Object(Plain,_) ] -> printf "<plain-object>" 
    | [ PrimitiveFunctionValue prim ] -> printf "<prim:%s>" <| Primitives.toString prim
    | _ -> printfn ""

let readLine args =
    if List.length args > 0 then
        failwith "Readline expects no arguments!"
    let line = System.Console.ReadLine()
    Object(Str line,ref Map.empty)

let deString =
    function 
    | [ Object(Str s,_) ] -> s
    | _ -> failwith "Argument must be a string"

let stringComp f = 
    function
    | [ Object(Str s,_); Object(Str x,_) ] -> f s x
    | _ -> failwith "Argument must be a string"

let substr = 
    function 
    | [ Object(Str str,_); Int s; Int e ] -> 
        Object(Str <| str.Substring(s,e-1),ref Map.empty)
    | _ -> failwith "Argument must be a string followed by two ints"

let getCtor = 
    function 
    | [ Object(_,slots); ctor ] -> 
        let slots = !slots
        match slots.TryFind "constructor" with
        | Some c -> c, ctor
        | None -> failwith "Constructor not found in fields!" 
    | _ -> failwith "Argument must be an object"

module private Primitives =
    let plus = binop (+) (+)
    let minus = binop (-) (-)
    let div = binop (/) (/)
    let prod = binop (*) (*)
    let lt = bincompop (<) (<)
    let ltEqual = bincompop (<=) (<=)
    let gt = bincompop (>) (>)
    let gtEqual = bincompop (>=) (>=)
    let stringEqual = stringComp (=) >> Bool
    let stringLt = stringComp (<) >> Bool

let callPrim func args =
    match func with
    | Primitive.Plus -> Primitives.plus args
    | Primitive.Minus -> Primitives.minus args
    | Primitive.Div -> Primitives.div args
    | Primitive.Prod -> Primitives.prod args
    | Primitive.LessThan -> Primitives.lt args
    | Primitive.GreaterThan -> Primitives.gt args
    | Primitive.LessThanEqual -> Primitives.ltEqual args
    | Primitive.GreaterThanEqual -> Primitives.gtEqual args
    | Primitive.And -> boolop (&&) args
    | Primitive.Or -> boolop (||) args
    | Primitive.Not -> boolunop (not) args
    | Primitive.StringLen -> deString >> String.length >> Int <| args
    | Primitive.StringAppend -> stringComp (fun f s -> Object(Str (f + s),ref Map.empty)) args
    | Primitive.String_Equal_Huh -> Primitives.stringEqual args
    | Primitive.String_LessThan_Huh -> Primitives.stringLt args
    | Primitive.Substring -> substr args
    | Primitive.Equal -> eqop args
    | Primitive.InstanceOf -> getCtor >> obj.ReferenceEquals >> Bool <| args
    | Primitive.Interger_Huh -> (function [ Int _ ] -> true | _ -> false) >> Bool <| args
    | Primitive.Boolean_Huh -> (function [ Bool _ ] -> true | _ -> false) >> Bool <| args
    | Primitive.Float_Huh -> (function [ Float _ ] -> true | _ -> false) >> Bool <| args
    | Primitive.Void_Huh -> (function [ Void ] -> true | _ -> false) >> Bool <| args
    | Primitive.String_Huh -> (function [ Object(Str _,_) ] -> true | _ -> false) >> Bool <| args
    | Primitive.Closure_Huh -> (function [ Object(Closure _,_) ] -> true | _ -> false) >> Bool <| args
    | Primitive.Plain_Huh -> (function [ Object(Plain,_) ] -> true | _ -> false) >> Bool <| args
    | Primitive.Print -> print args; Void
    | Primitive.ReadLine -> readLine args
    | Primitive.None | _ -> failwith "Invalid Primitive Function"