module Parser
open Ast
open FParsec.Primitives
open FParsec.CharParsers
open Ast.Primitives

type Parser<'a> = Parser<'a,unit>

(* Custom Combinators to filter primitives and "this" usage *)
let nonPrimitive (p: Parser<string,'u>) : Parser<string,'u> = 
    fun s ->
        let reply = p s
        if reply.Status = Ok then
            if isPrimitive reply.Result then
                fail "Cannot use a primitive as an identifier" s
            else reply
        else reply

let nonThis (p: Parser<string,'u>) : Parser<string,'u> =
    fun s ->
        let reply = p s
        if reply.Status = Ok then
            if reply.Result = "this" then
                fail "Cannout use 'this' as an identifier" s
            else reply
        else reply

let ensureDistinct (p:Parser<string list,'u>) : Parser<string list,'u> = 
    fun state -> 
        let reply = p state
        if reply.Status = Ok then 
            let res = reply.Result
            if List.length res <> (Set.count (Set.ofList res)) then
                failFatally "Parameter names must be distinct" state
            else reply
        else reply

(* Identifier *)
let identifier : Parser<_> = 
    let start = letter <|> anyOf "_$?" <?> "idenfitier"
    let rest = start <|> digit |> manyChars <?> "idenfitier"
    pipe2 (start |>> string) rest (+) 

let nonThisIdentifier = nonThis identifier
let unreservedIdentifier = nonPrimitive nonThisIdentifier 

let stmt, stmtRef = createParserForwardedToRef<Expression,unit>() 
let expr, exprRef = createParserForwardedToRef<Expression,unit>()

let stmts = many stmt |>> Sequence
let semi = spaces >>? skipChar ';' >>. spaces

(* ------------------------------------------------------------------------ 
    Statements
   ------------------------------------------------------------------------ *)

(* ----------------------
    Mutate statement:
        x = 3;
   ---------------------- *)
let mutate = 
    let name = nonThisIdentifier .>>? spaces .>>? followedBy (skipChar '=' .>> notFollowedByChar '=')
    let value = skipChar '=' >>. spaces >>. expr .>> semi
    tuple2 name value |>> Mutate <?> "variable mutation"

(* ----------------------
    Variable binding:
        var x = 3;
        ...
   ---------------------- *)
let bind = 
    let name = skipString "var" >>. spaces1 >>. unreservedIdentifier .>> spaces
    let value = skipChar '=' >>. spaces >>. expr .>> semi
    tuple3 name value stmts |>> Binding <?> "variable binding"

(* ----------------------
    Return statement:
        return 3;
   ---------------------- *)
let ret = skipString "return" .>> spaces1 >>. expr .>> semi |>> Return <?> "return statement"

let parenedTest = spaces >>. skipChar '(' >>. spaces >>. expr .>> spaces .>> skipChar ')' .>> spaces

(* { stmts } *)
let block = skipChar '{' >>. spaces >>. stmts .>> spaces .>> skipChar '}' .>> spaces

(* ----------------------
    If statement:
        if(...) 
        { ... }
        else 
        { ... }
   ---------------------- *)
let ifStatement = 
    let tst = skipString "if" >>. parenedTest
    let thn = block
    let els = skipString "else" >>. spaces >>. block
    pipe3 tst thn (opt els)
        (fun tst thn -> 
            function 
            | Some els -> If(tst,thn,els)
            | None -> If(tst,thn,Sequence [])) <?> "if statement"

(* ----------------------
    While statement:
        while(...) 
        { ... }
   ---------------------- *)
let whileStatement =
    let tst = skipString "while" >>. parenedTest
    tuple2 tst block |>> While <?> "while loop"

(* ----------------------
    Function def:
        function f(a,b,...) 
        { ... }
   ---------------------- *)
let func = 
    let name = skipString "function" >>. spaces1 >>. unreservedIdentifier .>> spaces <?> "function name"
    let parameters = between (skipChar '(' .>> spaces) (skipChar ')') 
                        ((sepBy (unreservedIdentifier .>> spaces) (skipChar ',' >>. spaces)) |> ensureDistinct) .>> spaces
    let body = block <?> "function body"
    pipe3 name parameters block (fun name parameters block -> (name, Func(parameters,block)))

(* ----------------------
    Function bindings:
        function f(a,b,...) 
        { ... }
        function ... () {}
        ...
   ---------------------- *)
let funcs = tuple2 (many1 func) stmts |>> FunctionBindings  <?> "function bindings"

(* ------------------------------------------------------------------------ 
    Expressions
   ------------------------------------------------------------------------ *)

let numOptions = NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger 
let number = 
    numberLiteral numOptions "Number" 
    |>> (fun nl -> 
            nl.String
            |> if nl.IsInteger then int >> IntConst
               else float >> FloatConst)
let boolean = (skipString "true" >>% true) <|> (skipString "false" >>% false) |>> BoolConst 
let stringLiteral =
    let escape =  
        anyOf "\"\\/bfnrt" <?> "escape character"
        |>> function
          | 'b' -> '\b'
          | 'f' -> '\u000C'
          | 'n' -> '\n'
          | 'r' -> '\r'
          | 't' -> '\t'
          | c   -> c 
    let quo = skipChar '"'
    between quo quo (manyChars (noneOf "\"\\" <|> (skipChar '\\' >>. escape))) <?> "string literal"
    |>> Ast.StringConst 

let literal = number <|> boolean <|> stringLiteral <??> "Literal"
let variableRef = identifier |>> VariableRef <?> "Variable reference"

let openParen = skipChar '(' >>. spaces
let closeParen = skipChar ')'
let argsList =  sepBy (expr .>> spaces) (skipChar ',' >>. spaces)
let args = spaces >>? between openParen closeParen argsList

(* ----------------------
    id Expression (anything that starts with an id):
        f
        f(...)
   ---------------------- *)
let idExpr =
    pipe2 identifier (opt args)
        (fun id ->
            function 
            | Some args -> 
                match id with
                | Primitive p -> FunctionApplication(PrimitiveFunction p, args)
                | _ -> FunctionApplication(VariableRef id,args)
            | None -> VariableRef(id))

(* ----------------------
    paren Expression:
        (...)
        (f())()
   ---------------------- *)
let parenExpr = 
    let parens = openParen >>. expr .>> spaces .>> closeParen
    pipe2 parens (opt args)
        (fun e ->
            function
            | Some args -> FunctionApplication(e,args) 
            | None -> e)

(* ----------------------
    member Expression:
        f.x
        f.x()
   ---------------------- *)
let memberExpr =
    let dot = spaces >>? skipChar '.' >>? spaces
    let ident = dot >>? identifier .>>? notFollowedBy (spaces >>. skipChar '=' .>> notFollowedByChar '=') <?> "member access"
    pipe2 ident (opt args <?> "method call")
        (fun mem ->
            function 
            | Some args -> fun obj -> MethodCall(obj,mem,args)
            | None -> fun obj -> MemberAccess(obj,mem))

(* ----------------------
    member binding Expression (any expression followed by a member mutation):
        f.x = 3;
        f.x().f = 4;
   ---------------------- *)
let exprBind = 
    let assignment = tuple2 (spaces >>? skipChar '.' >>. spaces >>. identifier .>> spaces) (skipChar '=' .>> spaces >>. expr)
    pipe2 expr ((opt assignment) .>> semi)
        (fun e ->
            function
            | Some (mem,value) -> MemberBind(e,mem,value)
            | None -> e) 

(* ----------------------
    New Expression:
        new Pair(1,2);
   ---------------------- *)
let newObj = tuple2 (skipString "new" .>>? spaces1 >>. variableRef) args |>> New

let primApp s =
    fun lhs rhs -> FunctionApplication(PrimitiveFunction (primitive s),[lhs; rhs])

let op s = spaces >>? pstring s .>> spaces |>> primApp <?> "binary operator"
let ops ops = 
    let opChoice = ops |> List.map pstring |> choice
    spaces >>? opChoice .>> spaces |>> primApp <?> "binary operator"


(* -----------------------
    Expression Definition 
   ----------------------- *) 
let expr' =
    choiceL [
        literal;
        newObj;
        idExpr;
        parenExpr;
    ] "expression"

let dotExpr' = many1FoldApply2 id (fun x f -> f x) id expr' memberExpr
let primNot e = FunctionApplication(PrimitiveFunction Primitive.Not,[ e ])
let notExpr' = pipe2 (manyFold id (fun f () -> fun e -> f (primNot e)) (skipChar '!' >>. spaces) <|>% id ) dotExpr' 
                (fun not e -> not e)
let prodExpr' = chainl1 notExpr' (op "*" )
let addSubExpr' = chainl1 prodExpr' (ops ["+"; "-"])
let divExpr' = chainl1 addSubExpr' (op "/")
let compExpr' = chainl1 divExpr' (ops ["<="; "<"; ">="; ">"])
let andOrExpr' = chainl1 compExpr' (ops ["&&"; "||"])
let eqExpr' = chainl1 andOrExpr' (op "==")

exprRef := eqExpr'

(* -----------------------
    Statement Definition 
   ----------------------- *)
stmtRef := 
    choiceL [
        ifStatement;
        funcs;
        ret;
        whileStatement;
        bind;
        mutate;
        exprBind;
    ] "statement"

(* -----------------------
    Program Definition 
   ----------------------- *)
let Program = stmts .>> eof

(* -----------------------
    Parser Method Definitions
   ----------------------- *)

let parseAst parser s =
    run parser s
    |> function
        | Success(ast,_,_) -> Some ast
        | f -> eprintfn "%A" f; None

let parseAstFrom parser (s:System.IO.TextReader) =
    parseAst parser (s.ReadToEnd())

let parse s = parseAst Program s
let parseFrom input = parseAstFrom Program input
let parseExpr s = parseAst expr s
