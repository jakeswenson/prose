module Interpreter
open Ast

exception RetEx of Value        

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

Ast.Xml.AstSerializer(input=stdin).Deserialize()
|> (fun exp -> interp exp [])
//|> (fun res -> eprintfn "Return Value = %A" res)
|> ignore