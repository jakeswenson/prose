namespace Ast

///This Enum reprersents the possible Language Primitives
type Primitive =
    | None = 0
    | Plus = 1
    | Minus = 2
    | Prod = 3
    | Div = 4
    | Sqrt = 28
    | LessThan = 5
    | LessThanEqual = 6
    | GreaterThan = 7
    | GreaterThanEqual = 8
    | And = 9
    | Or = 10
    | Not = 11
    | StringLen = 12
    | Substring = 13
    | StringAppend = 14
    | String_Equal_Huh = 15
    | String_LessThan_Huh = 16
    | Equal = 17
    | InstanceOf = 18
    | Interger_Huh = 19
    | Boolean_Huh = 20
    | Float_Huh = 21
    | Void_Huh = 22
    | String_Huh = 23
    | Closure_Huh = 24
    | Plain_Huh = 25
    | Print = 26
    | ReadLine = 27

module Primitives =
    let private primitives = 
        [   "+", Primitive.Plus;
            "-", Primitive.Minus; 
            "*", Primitive.Prod;
            "/", Primitive.Div;
            "==", Primitive.Equal;
            "<", Primitive.LessThan;
            ">", Primitive.GreaterThan;
            "<=", Primitive.LessThanEqual;
            ">=", Primitive.GreaterThanEqual;
            "||", Primitive.Or;
            "&&", Primitive.And;
            "!", Primitive.Not;
            "stringLength", Primitive.StringLen;
            "subString", Primitive.Substring;
            "stringEqual?", Primitive.String_Equal_Huh;
            "stringAppend", Primitive.StringAppend;
            "stringLessThan?", Primitive.String_LessThan_Huh;
            "instanceof", Primitive.InstanceOf;
            "int?", Primitive.Interger_Huh;
            "float?", Primitive.Float_Huh;
            "bool?", Primitive.Boolean_Huh;
            "void?", Primitive.Void_Huh;
            "string?", Primitive.String_Huh;
            "closure?", Primitive.Closure_Huh;
            "plain?", Primitive.Plain_Huh;
            "print", Primitive.Print;
            "readLine", Primitive.ReadLine;
            "sqrt", Primitive.Sqrt;
        ] |> Map.ofList

    let private reverseMap = 
        primitives 
        |> Map.toSeq 
        |> Seq.map (fun (x,y) -> (y,x)) 
        |> Map.ofSeq


    let argCount f =
        match f with
        | Primitive.Substring -> 3
        | Primitive.Plus | Primitive.Minus | Primitive.Prod | Primitive.Div 
        | Primitive.Equal | Primitive.LessThanEqual | Primitive.LessThan 
        | Primitive.GreaterThan | Primitive.GreaterThanEqual
        | Primitive.And | Primitive.Or | Primitive.StringAppend
        | Primitive.String_Equal_Huh | Primitive.String_LessThan_Huh | Primitive.InstanceOf
            -> 2
        | Primitive.Print | Primitive.Not | Primitive.StringLen 
        | Primitive.Boolean_Huh | Primitive.Closure_Huh
        | Primitive.Interger_Huh | Primitive.String_Huh
        | Primitive.Float_Huh | Primitive.Plain_Huh
        | Primitive.Void_Huh | Primitive.Sqrt
            -> 1
        | Primitive.ReadLine -> 0
        | Primitive.None | _ -> failwith "Unknown primitive"

    ///Determines if a string is a primitive
    let isPrimitive s = primitives.ContainsKey s

    ///Converts a string to a primitive
    let primitive s = primitives.[s]

    ///Converts a primitive to a string
    let toString p = reverseMap.[p]

    ///Active pattern to match a primitive
    let (|Primitive|_|) s = primitives.TryFind s

///Union type for expression's
type Expression =
    | IntConst of  int
    | FloatConst of float
    | BoolConst of bool
    | StringConst of string
    | VariableRef of string
    | If of Expression * Expression * Expression (*   test, then, else   *)
    | FunctionApplication of Expression * Expression list
    | Sequence of Expression list
    | Binding of string * Expression * Expression
    | PrimitiveFunction of Primitive
    | Func of string list * Expression (* params, body *)
    | FunctionBindings of (string * Expression) list * Expression
    | Return of Expression
    | Mutate of string * Expression
    | While of Expression * Expression
    | MemberAccess of Expression * string
    | MemberBind of Expression * string * Expression
    | MethodCall of Expression * string * Expression list 
    | New of Expression * Expression list

/// Value union for the result of interpretation
type Value = 
    | Bool of bool
    | Int of int
    | Float of float
    | Void
    | Object of ObjectValue * Map<string,Value> ref
    | PrimitiveFunctionValue of Primitive
    override t.ToString() = sprintf "%A" t

(* two ways to store the field information; going to try Refs, 
   but a Custom record type might be more elegant (as refs are just records anyways)
   A custom record will probably work better when we have to support prototypal inheritance
and Slots = { mutable Fields: Map<string,Value> } *)
and ObjectValue = 
    | Plain
    | Closure of string list * Expression * Environment list
    | Str of string
    override this.ToString() =
        match this with 
        | Plain -> "Plain"
        | Closure(parameters,body,env) -> sprintf "Closure(%A,%A,%A)" parameters body env
        | Str s -> sprintf "%A" s

and Environment = Env of string * Value option ref

