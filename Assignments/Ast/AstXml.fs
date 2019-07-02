module Ast.Xml

open System.Xml
open System
open System.IO
open Ast
open Reader


let private readFromReader (reader:XmlReader) =
    if reader.ReadState = ReadState.Initial then //Move to first element
        reader.Read() |> ignore 
    if reader.NodeType = XmlNodeType.XmlDeclaration then //Skip the <?xml...?> junk if its there
        reader.Read() |> ignore
    reader.ReadStartElement()
    let ast = read reader
    reader.ReadEndElement()
    ast

let rec private writeTo (writer:XmlWriter) ast =
    let write name contents = writer.WriteElementString(name,contents)
    let writeAst = writeTo writer
    let start name = writer.WriteStartElement(name)
    let end'() = writer.WriteFullEndElement()
    match ast with
    | IntConst i      -> write "LitInt"   <| sprintf "%i" i
    | BoolConst b     -> write "LitBool"  <| sprintf "%b" b
    | FloatConst f    -> write "LitFloat" <| sprintf "%f" f
    | VariableRef var -> write "Varref" var
    | StringConst s   -> 
        start "LitStr"
        writer.WriteCData(s)
        end'()
    | If(tst,thn,els) -> 
        start "If"
        writeAst tst 
        writeAst thn
        writeAst els
        end'()
    | FunctionApplication(func,args) ->
        start "Application"
        writeAst func
        List.iter writeAst args
        end'()
    | Sequence exprs ->
        start "Sequence"
        List.iter writeAst exprs
        end'()
    | Binding(name,value,body) ->
        start "VarBind"
        write "VarName" name
        writeAst value
        writeAst body
        end'()
    | FunctionBindings(funcs,body) -> 
        start "FunBind"
        funcs
        |> List.iter (fun (name,func) -> 
                        start "FunBinding"
                        write "Name" name
                        writeAst func
                        end'())
        writeAst body
        end'()
    | Func(parameters,body) ->
        List.iter (write "Param") parameters
        writeAst body
    | Return value -> 
        start "Return"
        writeAst value
        end'()
    | Mutate(name,value) ->
        start "SetVar" 
        write "VarSetName" name
        writeAst value
        end'()
    | While(tst,body) -> 
        start "While"
        writeAst tst
        writeAst body
        end'()
    | MemberAccess(obj,name) -> 
        start "FieldRef"
        writeAst obj
        write "FieldRefName" name
        end'()
    | MemberBind(obj,name,value) -> 
        start "FieldSet"
        writeAst obj
        write "FieldSetName" name
        writeAst value
        end'()
    | MethodCall(obj,name,args) ->
        start "FieldCall"
        writeAst obj
        write "FieldCalledName" name
        List.iter writeAst args
        end'()
    | New(ctor,args) -> 
        start "NewExp"
        writeAst ctor
        List.iter writeAst args
        end'()
    | PrimitiveFunction prim -> 
        writeAst (VariableRef <| Primitives.toString prim)

type AstSerializer(?input,?output) =
    let input = defaultArg input null
    let output = defaultArg output null
    member this.Deserialize() = 
        let settings = XmlReaderSettings(IgnoreWhitespace=true,IgnoreComments=true,CloseInput=true)
        use reader = XmlReader.Create((input:TextReader),settings)
        readFromReader reader
    member this.Serialize(ast) =
        let settings = XmlWriterSettings(Indent=true,CloseOutput=true)
        use writer = XmlWriter.Create((output:TextWriter),settings)
        writer.WriteStartElement("Program")
        writeTo writer ast
        writer.WriteFullEndElement()
        
        
AstSerializer(input=stdin).Deserialize()
|> printfn "%A"
