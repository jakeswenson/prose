module Tests
open Ast
open Interpreter
open Xunit

module Common =
    let private i = 8
    let private f = 8.8
    let private s = "testing"

    let VoidAnswer = Void

    let IntC = IntConst i
    let IntAnswer = Int i

    let XRef = VariableRef "x"
    
    let FloatConstant = FloatConst f
    let FloatAnswer = Float f

    let StringConstant = StringConst s
    let StringAnswer = Object(Str s,ref Map.empty)

    let TrueConstant = BoolConst true
    let TrueAnswer = Bool true
    
    let FalseConstant = BoolConst false
    let FlaseAnswer = Bool false

module AST = 
    let func parameters body = Func(parameters,body)
    let ufunc body = func [] body
    let funcs functions body = FunctionBindings(functions,body)
    let app func args = FunctionApplication(func,args)
    let uapp func = app func []
    let appPrim prim args = FunctionApplication(PrimitiveFunction prim,args) 
    let appbinop prim lhs rhs = appPrim prim [lhs;rhs]
    let appunop prim arg = appPrim prim [arg] 
    let call obj name args = MethodCall(obj,name,args)
    let ucall obj name = call obj name []
    let bindMem obj (name, value) = MemberBind(obj,name,value)
    let bind (name, value) body = Binding(name,value,body)
    let ret value = Return value
    let newObj ctor args = New(ctor,args)
    let memAcc obj name = MemberAccess(obj,name)
    let memBind obj name value = MemberBind(obj,name,value)
    let seq exps = Sequence exps
    let if' tst thn els = If(tst,thn,els)
    let if'b tst thn els = If(BoolConst tst,thn,els)
    let mutate x newVal = Mutate(x,newVal)
    let while' tst body = While(tst,body)
    let while'range var from to' body = 
        bind (var, IntConst from) (while' (appPrim Primitive.LessThan [VariableRef var; IntConst to']) body)
    let and' x y = appPrim Primitive.And [ x; y ]
    let or' x y  = appPrim Primitive.Or [ x; y ]
    let not x  = appPrim Primitive.Not [ x; ]
    let ref s = VariableRef s 

    let str s = StringConst s
    let int i = IntConst i
    let this = ref "this"

module Vals = 
    let fields slots = ref <| Map.ofList slots
    let plain slots = Object(Plain,fields slots)
    let string s slots = Object(Str s, fields slots)
    let str s = Object(Str s, fields [])
    let closure parameters body env slots  = Object(Closure(parameters,body,env),fields slots)
    let clo body = closure [] body [] []
    let pclo ps body = closure ps body [] []
    let int i = Int i

let test x answer = 
    let res = interp x []
    Assert.Equal(res,answer)

open FsxUnit.Syntax

let eval exp = interp exp []

[<Fact>]
let ``Ints should eval to Ints``() = 
    eval Common.IntC
    |> should equal Common.IntAnswer

[<Fact>]
let ``Floats should eval to floats``() = 
    eval Common.FloatConstant 
    |> should equal Common.FloatAnswer

[<Fact>]
let ``Bools should eval to bools``() = 
    eval Common.TrueConstant 
    |> should equal Common.TrueAnswer 

[<Fact>]
let ``Strings should eval to strings``() = 
    eval Common.StringConstant
    |> should equal Common.StringAnswer

[<Fact>]
let ``Binding X to 8 should equal 8``()= 
    let b = AST.bind ("x", Common.IntC) Common.XRef
    eval b
    |> should equal Common.IntAnswer

[<Fact>]
let ``Applying a function with no return should return void``() =
    let app = AST.app <| AST.func ["x"] Common.XRef <| [Common.IntC]
    eval app
    |> should equal Common.VoidAnswer

[<Fact>]
let ``Applying a function returning 8 should equal 8``() =
    let app1 = AST.func ["x"] <| AST.ret Common.XRef |> AST.app <| [Common.IntC]
    eval app1
    |> should equal Common.IntAnswer

[<Fact>]
let ``8 + 8 should equal 16``() =
    let plus = AST.appPrim Primitive.Plus [ Common.IntC; Common.IntC ]
    eval plus
    |> should equal (Int 16)

[<Fact>]
let ``8 - 8 should equal 0``() =
    let plus = AST.appPrim Primitive.Minus [ Common.IntC; Common.IntC ]
    eval plus
    |> should equal (Int 0)

[<Fact>]
let ``8.8/8.8 should equal 1.0``() =
    let plus = AST.appPrim Primitive.Div [ Common.FloatConstant; Common.FloatConstant]
    eval plus
    |> should equal (Float 1.0)

[<Fact>]
let ``8.8 / 8 should equal 1.1``() =
    eval (AST.appPrim Primitive.Div [ Common.FloatConstant; Common.IntC])
    |> should equal (Float 1.1)

[<Fact>]
let ``Printing "Testing" to the console should return void``() =
    let o = AST.seq [ AST.appPrim Primitive.Print [ AST.str "Testing" ] ]
    let output = System.Text.StringBuilder()
    let out = System.Console.Out
    use sw = new System.IO.StringWriter(output)
    System.Console.SetOut(sw)
    eval o
    |> should equal Common.VoidAnswer
    output.ToString()
    |> should contain ">>> Testing" 
    System.Console.SetOut(out)

[<Fact>]
let ``Reading a line from the console should read a line``() =
    let test = "testing"
    let cIn = System.Console.In;
    use sr = new System.IO.StringReader(test)
    System.Console.SetIn(sr)
    let o = AST.appPrim Primitive.ReadLine []
    eval o
    |> should equal (Vals.str test)
    System.Console.SetIn(cIn)

let newOCtor = AST.func [] <| AST.bindMem AST.this ("x", Common.IntC)
let newO = AST.newObj newOCtor []

[<Fact>]
let ``Creating a new object should bind this and run and set the constructor``() =    
    let newOa = Vals.plain ["x",Common.IntAnswer;"constructor", Vals.clo <| MemberBind(AST.this,"x",Common.IntC) ]
    eval newO
    |> should equal newOa

[<Fact>]
let ``Accessing a member should return its value``() =
    let getX = AST.memAcc newO "x"
    eval getX
    |> should equal Common.IntAnswer

[<Fact>]
let ``Binding a method with no return to a member of an object should be callable and return void``() = 
    let methodCall = AST.bind ("x", newO) <| AST.seq [ AST.bindMem Common.XRef ("invoke", AST.ufunc Common.IntC); AST.ucall Common.XRef "invoke" ]
    eval methodCall
    |> should equal Common.VoidAnswer

[<Fact>]
let ``Calling a method returning 8 should return 8``() =
    let methodCallWithResult = 
        AST.bind ("x", newO) 
        <| AST.seq [ AST.bindMem Common.XRef ("invoke", AST.ret Common.IntC |> AST.ufunc); 
                     AST.ucall Common.XRef "invoke" ]
    eval methodCallWithResult
    |> should equal Common.IntAnswer

[<Fact>]
let ``A free variable should fail``() =
    lazy( eval Common.XRef )
    |> should fail_with "Unbound Variable"

[<Fact>]
let ``If an If's test is not a bool it should fail``() =
    let ifExp = AST.if' Common.IntC Common.IntC Common.IntC
    lazy( eval ifExp )
    |> should fail_with "Test did not produce a boolean value!"

[<Fact>]
let ``If an If's test is true the then branch should run``() = 
    let ifExp = AST.if'b true Common.IntC Common.FloatConstant
    eval ifExp
    |> should equal Common.IntAnswer

[<Fact>]
let ``If an If's test is false the else branch should run``() = 
    let ifExp = AST.if'b false Common.IntC Common.FloatConstant
    eval ifExp
    |> should equal Common.FloatAnswer

[<Fact>]
let ``An empty sequence should produce void``() =
    eval <| AST.seq []
    |> should equal Void

[<Fact>]
let ``A sequence should produce the last value``() =
    eval <| AST.seq [ Common.IntC; Common.FloatConstant ]
    |> should equal Common.FloatAnswer

[<Fact>]
let ``Returning within a return should fail``() =
    lazy( eval (AST.ret <| AST.ret Common.IntC) )
    |> should fail_with "You can't return from inside a return!!"

[<Fact>]
let ``Returning outside of a function should fail``() =
    lazy( eval <| AST.ret Common.IntC )
    |> should fail_with "You cannot return outside of a function!"

[<Fact>]
let ``After binding x to 8 and then setting it to 8.8, it should equal 8.8``() =
    eval (AST.bind ("x", Common.IntC) <| AST.seq [ AST.mutate "x" Common.FloatConstant; Common.XRef ] )
    |> should equal Common.FloatAnswer

[<Fact>]
let ``Accessing a member on a non object should fail``() = 
    lazy( eval <| AST.memAcc Common.IntC "x" )
    |> should fail_with "Member access requires an object."

[<Fact>]
let ``The string length of "test" should equal 4``() =
    eval <| AST.appPrim Primitive.StringLen [AST.str "test"]
    |> should equal (Int 4)

[<Fact((* Skip="I dont know what this should do?" *))>]
let ``The substring of "test" from 1 to 3 should equal "es"``() =
    eval <| AST.appPrim Primitive.Substring [AST.str "test"; IntConst 1; IntConst 3]
    |> should equal (Vals.str "es")

[<Fact>]
let ``While x from 0 to 10 print x``() = 
    let body = AST.seq [ AST.appPrim Primitive.Print [ Common.XRef ]; AST.mutate "x" (AST.appPrim Primitive.Plus [ Common.XRef; IntConst 1 ]) ]
    let while' = AST.while'range "x" 0 10 body 
    let output = System.Text.StringBuilder()
    let out = System.Console.Out
    use sw = new System.IO.StringWriter(output)
    System.Console.SetOut(sw)
    eval while'
    |> should equal Void
    System.Console.SetOut(out)
    output.ToString()
    |> should match' "(?s:^\D*0.*1.*2.*3.*4.*5.*6.*7.*8.*9\D*$)" 
    
let ``The instance of the string "test" should be string``() = ()

[<Fact>]
let ``If the airity of a function application differs it should fail``() = 
    lazy( AST.func [] <| AST.ret Common.TrueConstant
    |> AST.app <| [ Common.IntC ]
    |> eval )
    |> should fail_with "Function arity mismatch"

[<Fact>]
let ``Mutually recursive functions should work``() = 
    let eRef = AST.ref "even?"
    let oRef = AST.ref "odd?"
    
    let decX = AST.appPrim Primitive.Minus [ Common.XRef; AST.int 1 ]
    let xEqual0 = AST.appPrim Primitive.Equal [ Common.XRef; AST.int 0]
    let eElse = AST.app oRef [ decX ]
    let eBody = AST.if' xEqual0 Common.TrueConstant eElse    
    let oElse = AST.app eRef [ decX ]
    let oBody = AST.if' xEqual0 Common.FalseConstant oElse
                 
    let even = AST.func ["x"] (AST.ret eBody)
    let odd = AST.func ["x"] (AST.ret oBody)
    let binds = AST.funcs ["even?",even;"odd?",odd] (AST.and' (AST.app oRef [AST.int 13]) 
                                                        (AST.and' (AST.not (AST.app eRef [AST.int 13]))
                                                                  (AST.app eRef [AST.int 14])))
    eval binds 
    |> should equal Common.TrueAnswer

[<Fact>]
let ``Example from class``() =
    let pairBody = AST.bind ("pair", AST.func ["a";"b"] <| AST.seq [ AST.memBind AST.this "fst" <| AST.ref "a"; AST.memBind AST.this "rst" <| AST.ref "b" ])
    let bindings = AST.bind ("x",AST.int 5) 
    let x = bindings (AST.funcs [ "getX", AST.ufunc <| AST.ret (AST.ref "x");
                                  "setX", AST.func ["newX"] <| AST.mutate "x" (AST.ref "newX") ] 
                       ( AST.ret (AST.newObj (AST.ref "pair") [AST.ref "getX"; AST.ref "setX"] ) ))
    
    let f = AST.ufunc x |> AST.uapp |> pairBody

    let pair = AST.bind ("p",f)
    let getX = pair (AST.seq [ AST.ucall (AST.ref "p") "fst"; ]) 
    eval getX
    |> should equal (Vals.int 5)

    let setX = pair (AST.seq [ AST.call (AST.ref "p") "rst" [ AST.int 33 ]; AST.ucall (AST.ref "p") "fst";])
    eval setX
    |> should equal <| Vals.int 33

    let func = 
        AST.ufunc 
        <| AST.bind ("x", AST.int 5) 
            (AST.bind ("y", AST.int 6) 
              (AST.ufunc (AST.appbinop Primitive.Plus (AST.ref "x") (AST.ref "y") |> AST.ret) |> AST.ret ) )

    let app = AST.uapp func |> AST.uapp
    eval app
    |> should equal (Vals.int 11)

[<Fact>]
let ``Factorial should work``() =
    let fact = AST.func ["x"] <| AST.if' (AST.appbinop Primitive.LessThanEqual (AST.ref "x") (AST.int 1)) (AST.ret (AST.int 1)) (AST.ret (AST.appbinop Primitive.Prod (AST.ref "x") (AST.app (AST.ref "f") [ AST.appbinop Primitive.Minus (AST.ref "x") (AST.int 1) ])))
    let facts = AST.funcs (("f",fact)::[]) (AST.app (AST.ref "f") [ AST.int 8 ])
    eval facts
    |> should equal ( List.reduce (*) [1 .. 8] |> Vals.int )

//let both f answer =
//    (fun a -> (fun _and -> f answer a
//                           _and a))
//let And s f =  s f 
//let ase = And should equal
//let a = ase Common.IntAnswer
//let sb = should both equal Common.IntAnswer |> a
