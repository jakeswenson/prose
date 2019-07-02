module Reader
open System.Xml
open System
open Ast
open Ast.Primitives

let rec read (reader:XmlReader) =
    let consume() = reader.ReadStartElement()
    let consumeEnd() = reader.ReadEndElement()
    match reader.LocalName with
    | "LitInt" -> 
        reader.ReadElementContentAsInt()
        |> IntConst
    | "LitStr" ->
        reader.ReadElementContentAsString()
        |> StringConst
    | "LitFloat" -> 
        reader.ReadElementContentAsDouble()
        |> FloatConst
    | "LitBool" -> 
        reader.ReadElementContentAsBoolean()
        |> BoolConst
    | "Varref" ->
        reader.ReadElementContentAsString()
        |> VariableRef
    | "If" -> 
        consume()
        let tst = read reader
        let thn = read reader
        let els = read reader
        consumeEnd()
        If(tst,thn,els)
    | "Application" ->
        consume()
        let fun' = 
            match read reader with
            | VariableRef(Primitive prim) -> PrimitiveFunction prim
            | e -> e
        let args = [ while reader.NodeType = XmlNodeType.Element do yield read reader ]
        consumeEnd()
        FunctionApplication(fun',args)
    | "Sequence" ->
        consume()
        let seq = [ while reader.NodeType = XmlNodeType.Element do yield read reader ]
        consumeEnd()
        Sequence seq
    | "VarBind" ->
        consume()
        let name = reader.ReadElementContentAsString()
        let value = read reader
        let body = read reader
        consumeEnd()
        Binding(name,value,body)
    | "FunBind" ->
        consume()
        let funcs = [ while reader.NodeType = XmlNodeType.Element && reader.LocalName = "FunBinding" do yield readFunction reader]
        let body = read reader
        consumeEnd()
        FunctionBindings(funcs,body)
    | "Return" ->
        consume()
        let r = read reader
        consumeEnd()
        Return r
    | "SetVar" -> 
        consume()
        let name = reader.ReadElementContentAsString()
        let value = read reader
        consumeEnd()
        Mutate(name,value)
    | "While" ->
        consume()//now: tst node
        let tst = read reader
        let body = read reader
        consumeEnd()
        While(tst,body)
    | "FieldRef" -> 
        consume()
        let obj = read reader
        let name = reader.ReadElementContentAsString()
        consumeEnd()
        MemberAccess(obj,name)
    | "FieldSet" -> 
        consume() //now: FieldSetName
        let obj = read reader
        let name = reader.ReadElementContentAsString()
        let value = read reader
        consumeEnd()
        MemberBind(obj,name,value)
    | "FieldCall" -> 
        consume()
        let obj = read reader
        let name = reader.ReadElementContentAsString()
        let args = [ while reader.NodeType = XmlNodeType.Element do yield read reader ]
        consumeEnd()
        MethodCall(obj,name,args)
    | "NewExp" ->
        consume()
        let ctor = read reader
        let args = [ while reader.NodeType = XmlNodeType.Element do yield read reader ]
        consumeEnd()
        New(ctor,args)
    | _ -> failwith "unknown node"

and readFunction reader =
    reader.ReadStartElement()
    //now on Name node
    let name = reader.ReadElementContentAsString()
    let parameters = [ while reader.LocalName = "Param" do yield reader.ReadElementContentAsString() ] 
    let body = read reader
    reader.ReadEndElement()
    (name,Func(parameters,body))
