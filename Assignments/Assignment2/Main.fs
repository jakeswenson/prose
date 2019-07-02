module private Main
open Parser
open Ast.Xml

let (|?>) (x:'t option) f = if x.IsSome then f x.Value

stdin
|> parseFrom
|?> AstSerializer(output=stdout).Serialize