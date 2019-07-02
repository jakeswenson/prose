open Parser

let parsep = parse >> printfn "%A"


parsep @"function f()
{
    function a(){ return f; }
    return ""test"";
    return a;
}
f();"

parsep "!f.f;"
parsep "4 + 5 / 7 == !f.f;"

parsep "f.f=4;"
parsep "\"test\".x=4;"
parsep "f=4;"
parsep "print(\"\\n\");"
parsep "a == 4;"
parsep "a.b == 4;"
parsep "a.b;";
parsep "a.b = 4;";
parsep "a + b * c / d;"
parsep "this.p;"
parsep "t.p=p;"
parsep "(4==4).b=4;"
parsep "this.a().x;"
parsep "this.a().x=3;"
parsep "var stringAppend = 3;"
parsep "var s = 3;"
parsep "print(3).f"
let e = parse "3 + 4 / ! c . abc * 6 == 5 + 6 + 7 && true || false;"
printfn "%A" e
let e2 = parse "((3 + 4) / ((! (c . abc)) * 6)) == ((((5 + 6) + 7) && true) || false);"
printfn "%A" e2
printfn "%b" (e = e2)

System.Console.ReadKey(true) |> ignore