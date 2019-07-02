module Parser

open System.IO
open Ast

/// Parses a Program from a string
val parse : string -> Expression option

/// Parses an Expression from a string
val parseExpr : string -> Expression option

/// Parses a Program from a TextReader
val parseFrom : TextReader -> Expression option
