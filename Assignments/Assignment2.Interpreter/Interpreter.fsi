module Interpreter

open Ast

val interp : Expression -> Environment list -> Value