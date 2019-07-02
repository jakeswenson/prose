module Interpreter
open Ast

exception RetEx of Value

module internal Helpers =
    let private binop func ffunc args = 
        match args with 
        | [Int lhs; Int rhs] -> func lhs rhs |> Int
        | [Float lhs; Float rhs] -> ffunc lhs rhs |> Float
        | [Float f; Int i] -> ffunc f (float(i)) |> Float
        | [Int i; Float f] -> ffunc (float(i)) f |> Float
        | _ -> failwith "Operands must be numeric"

    let private bincompop func ffunc args = 
        match args with 
        | [Int lhs; Int rhs] -> func lhs rhs |> Bool
        | [Float lhs; Float rhs] -> ffunc lhs rhs |> Bool
        | [Float f; Int i] -> ffunc f (float(i)) |> Bool
        | [Int i; Float f] -> ffunc (float(i)) f |> Bool
        | _ -> failwith "Operands must be numeric"

    let private eqop args = 
        match args with 
        | [Int lhs; Int rhs] -> lhs = rhs |> Bool
        | [Float lhs; Float rhs] -> lhs = rhs |> Bool
        | [Bool lhs; Bool rhs] -> lhs = rhs |> Bool
        | [Void as f; Void as v] -> f = v |> Bool
        | [Object(_,slots) ; Object(_,rhs)] -> obj.ReferenceEquals(!slots,!rhs) |> Bool
        | [ _ ; _ ] -> Bool false
        | _ -> failwith "== Expects two arguments"

    let private boolop func args = 
        match args with 
        | [ Bool b; Bool c] -> func b c |> Bool
        | _ -> failwith "Arguments should be two Booleans"

    let private boolunop func args = 
        match args with 
        | [ Bool b] -> func b |> Bool
        | _ -> failwith "Argument should be a Boolean"

    let private stringunop func args =
        match args with 
        | [ Object(Str s,slots) ] -> func s
        | _ -> failwith "Argument must be a string"

    let private print = 
        function 
        | [ Int i ] -> printfn ">>> %i" i
        | [ Float f ] -> printfn ">>> %f" f
        | [ Bool b ] -> printfn ">>> %b" b
        | [ Void ] -> printfn ">>> {void}"
        | [ Object(Str s,_) ] -> printfn ">>> %s" s
        | [ v ] -> printfn ">>> %A" v 
        | _ -> printfn ""

    let private readLine args =
        if List.length args > 0 then
            failwith "Readline expects no arguments!"
        let line = System.Console.ReadLine()
        Object(Str line,ref Map.empty)

    let private deString =
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

let rec private interp' exp env : Value = 
    let apply body parameters args cloenv = 
        try 
            if List.length args <> List.length parameters then
                failwithf "Function arity mismatch: attempting to apply %i paramters to a function expecting %i" (List.length args) (List.length parameters)
            args
            |> List.map (fun a -> interp' a env |> Some |> ref) 
            |> List.zip parameters
            |> List.map Env
            |> (fun env -> List.append env cloenv)
            |> interp' body |> ignore
            Void
        with
        | RetEx value -> value

    let lookup var env = 
        List.tryPick (function Env(name,value) as e when name = var -> Some e | _ -> None) env 
        
    match exp with
    | IntConst i -> Int i 
    | FloatConst f -> Float f 
    | BoolConst b -> Bool b
    | StringConst s ->  Object(Str s,ref Map.empty)
    | VariableRef var -> 
        match lookup var env with
        | Some(Env(name,{ contents = Some value })) -> value
        | _ -> failwithf "Unbound Variable: a variable named %s was not found" var
    | If(test,thn,els) -> 
        match interp' test env with
        | Bool true -> interp' thn env
        | Bool false -> interp' els env
        | _ -> failwithf "Test did not produce a boolean value! %A" test
    | FunctionApplication(func,args) -> 
        match interp' func env with
        | PrimitiveFunctionValue prim -> 
            List.map (fun a -> interp' a env) args
            |> Helpers.callPrim prim 
        | Object(Closure(parameters,body,cloenv),_) -> 
            apply body parameters args cloenv
        | _ -> failwith "Improper Function Application"
    | Sequence [] -> Void 
    | Sequence exps -> List.fold (fun _ exp -> interp' exp env) Void exps
    | Binding(str,value,body) -> 
        Env(str,interp' value env |> Some |> ref )::env 
        |> interp' body
    | Func(parameters,body) ->
        Object(Closure(parameters,body,env),ref Map.empty)
    | PrimitiveFunction prim -> PrimitiveFunctionValue prim
    | FunctionBindings(funcs,body) -> 
        let envs = List.map (fun (name,_) -> Env(name, ref None)) funcs
        let finalEnv = envs @ env
        funcs
        |> List.map (fun (_,func) -> interp' func finalEnv )
        |> List.iter2 (fun env value -> match env with Env(_,v) -> v := Some value) envs
        interp' body finalEnv
    | Return value -> 
        let res = 
            try 
                interp' value env 
            with
            | RetEx _ -> failwith "You can't return from inside a return!!"
        RetEx(res) |> raise
    | Mutate(var,newValue) -> 
        match lookup var env with
        | Some(Env(name,value)) -> 
            let newVal = interp' newValue env
            value := Some newVal
            newVal
        | _ -> failwithf "a variable named %s was not found" var
    | While(test,body) -> 
        while interp' test env |> (function Bool true -> true | Bool false -> false | _ -> failwith "While Test result not a Boolean!") do
            interp' body env |> ignore
        Void
    | MemberAccess(obj,field) -> 
        match interp' obj env with
        | Object(_,slots) -> 
            match (!slots).TryFind field with
            | Some value -> value
            | None -> failwithf "Member %s is undefined" field  
        | _ -> failwithf "Member access requires an object. Given: %A" obj
    | MemberBind(obj,field,newVal) -> 
        match interp' obj env with
        | Object(_,slotsRef) ->
            let slots =
                let slots = !slotsRef
                if slots.ContainsKey field then
                    Map.remove field slots
                else slots
            let v = interp' newVal env 
            slotsRef := Map.add field v slots
            v
        | _ -> failwithf "Member mutation requires an object. Given: %A" obj
    | MethodCall(obj,field,args) -> 
        match interp' obj env with
        | Object(_, slots) as obj -> 
            match (!slots).TryFind field with
            | Some(Object(Closure(parameters,body,env),_)) -> apply body parameters args (Env("this",ref <| Some obj)::env)
            | None -> failwithf "A method named %s was not found in %A" field !slots
            | _ -> failwith "Invalid method!"
        | _ -> failwithf "Member calls requires an object. Given: %A" obj
    | New(con,args) -> 
        match interp' con env with
        | Object(Closure(parameters,body,cloEnv),_) as con->
            let slots = ref Map.empty
            let o = Object(Plain,slots)
            let newCloEnv = Env("this",ref <| Some o)::cloEnv
            apply body parameters args newCloEnv |> ignore
            slots := Map.add "constructor" con !slots
            o
        | _ -> failwith "Not a constructor"

let interp exp env = 
    try
        interp' exp env
    with 
    | RetEx message -> failwith "You cannot return outside of a function!"
