module Ast.Xml

open Ast
open System.IO

type AstSerializer =
    new : ?input:TextReader * ?output:TextWriter -> AstSerializer
    member Deserialize : unit -> Expression
    member Serialize : Expression -> unit