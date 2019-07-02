#I "../../../public/jcswenso/FAKE"
#r "FakeLib.dll"
#r "System.Core.dll"

open System.IO
open Fake

type FscOptions = 
    { Includes : string list
      References : string list
      Output : string
      IsLibrary : bool
      Resident : bool }

let defaultFscOpts = 
    { Includes=[]; 
      References=[];
      Output=null;
      IsLibrary = false
      Resident = true }

let Fsc files optsf = 
    let { Includes=incs; 
          References=refs;
          Output=out
          IsLibrary=isDll
          Resident=res } = optsf defaultFscOpts

    let exec () = 
        ExecProcess (fun info ->
            info.FileName <- "mono"

            let opts = seq {
                yield! incs |> Seq.map ((+) "-I:") 
                yield! refs |> Seq.map ((+) "-r:")
                if out <> null then 
                    yield "--out:"+out
                if isDll then 
                    yield "-a"
                if res then
                    yield "--resident"
                yield! files
            }
            let cmd = sprintf "../../../../FSharp/bin/fsc.exe -O %s" (String.concat " " opts)
            trace cmd
            info.Arguments <- cmd
            info.UseShellExecute <- false
        ) |> ignore
    
    
    if out <> null then
        let updated = File.GetLastWriteTimeUtc >> ((<) (File.GetLastWriteTimeUtc out))  
        if Seq.exists updated files then
            trace "Files modified, Rebuilding!"
            exec()
        else 
            trace "Files not updated, Skipping build"
    else exec()
        
    

Target? Clean <- DoNothing
//    fun _ ->
//        trace "Cleaning"
//        !+ "*.dll"
//        ++ "*.exe"
//        |> Scan
//        |> Seq.iter DeleteFile

Target? Primitives <-
    fun _ ->
        trace "Building Primitives"
        ExecProcess (fun info -> 
            info.FileName <- "gcc"
            info.Arguments <- "-o primitives.o -lm -c primitives.c -m32"
        ) |> ignore

Target? Ast <- 
    fun _ ->
        trace "Building the Ast"
        Fsc (List.map ((+) "../Ast/") ["Ast.fs"; "NonMonadicReader.fs"; "AstXml.fsi"; "AstXml.fs" ]) 
            (fun opts -> 
                { opts with  
                    Output="ast.dll";
                    IsLibrary = true })


Target? Parser <- 
    fun _ ->
        trace "Building the Parser"
        Fsc (List.map ((+) "../Parser/") ["Parser.fsi"; "Parser.fs" ]) 
            (fun opts -> 
                { opts with 
                    Includes = ["../../../public/jcswenso/FParsec/"]; 
                    References = ["FParsec.dll"; "FParsecCS.dll"; "ast.dll" ]; 
                    Output="parser.dll";
                    IsLibrary = true })
        

Target? Compiler <-
    fun _ ->
        trace "Building Compiler"
        Fsc ["SMap.fs"; "StateMonad.fs"; "Ast\\'.fs"; "Analyze.fs"; "Locator.fs"; "LLVM.fs"; "IR.fs"; "Blocks.fs"; "Sparc.fs"; "Program.fs"] 
            (fun opts -> 
                { opts with 
                    Includes = ["../Ast/"]; 
                    References = ["ast.dll"; "parser.dll"]
                    Output="compiler.exe" })
    
Target? Build <- DoNothing

 
For? Ast <- 
    Dependency? Clean

For? Parser  <- 
    Dependency? Clean
    |> And? Ast

For? Compiler <-
     Dependency? Clean
     |> And? Ast
     |> And? Parser
     |> And? Primitives 

For? Build <- Dependency? Compiler 

Run? Build
