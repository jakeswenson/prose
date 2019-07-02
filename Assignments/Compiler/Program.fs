open Parser

open Ast'
open Analyze
open Locator
open IR
open System.IO


let logAnalyzerResults s e =
    eprintfn "\n----------------Analyzation Results------------------------"
    eprintfn "%A" s
    eprintfn "\n----------------Analyzation Expr------------------------"
    eprintfn "%A\n\n" e

let logLocationResults locfo e =
    eprintfn "\n----------------TopLevel Locs-----------------\n"
    eprintfn "%A" locfo
    eprintfn "----------------New Expr--------------------\n"
    eprintfn "%A\n\n" e

let debugPrint name f o =
    eprintfn "\n----------------%s-----------------\n" name
    eprintfn "%A" o
    eprintfn "\n----------------End %s--------------------\n" name
    f o

let debugPrint2 name f o o2 =
    eprintfn "\n----------------%s-----------------\n" name
    eprintfn "%A\n%A" o o2
    eprintfn "\n----------------End %s--------------------\n" name
    f o

[<EntryPoint>]
let main(args:string[]) =
    let verbose = Array.exists ((=) "-v") args
    let debugPrint s f o =
        if verbose then
            debugPrint s f o
        else f o 
    let debugPrint2 s o o2 =
        if verbose then 
            debugPrint2 s ignore o o2
        else ()
    if args.Length > 0 then
        if Array.exists ((=) "-debug") args then
            parse "var x = 3;
                    var s = \"test\";
                    print(s);
                    x + 4.9;
                    var y = 4.5;
                     x+ y/y*y;"
        else parseFrom (File.OpenText(args |> Array.find (fun s -> not(s.StartsWith("-")))))
    else parseFrom stdin
    |> function 
        | Some x -> 
            x
            |> Expression.Convert 
            |> fun  ast  -> analyze ast Utils.SMap.empty 
            |> debugPrint "Analyzation Results" id
            ||> fun varTypeScope expr -> locate expr (VarLocation.empty varTypeScope)
            |> debugPrint "Location Results" id
            |> fun (locfo,e) -> toIr e (IR.Generator.empty locfo), locfo
            ||> fun ({ Instructs = insts; Locs = locs },res) locfo ->
                if Array.exists ((=) "-sparc") args then
                    if verbose then
                        List.ofSeq insts 
                        |> debugPrint "Instructions" ignore
                        insts
                        |> Blocks.blockify "main"
                        |> (fun (blocks,firstBlock) ->
                            debugPrint "Basic Blocks" ignore blocks

                            Blocks.liveness List.empty firstBlock
                            ||> debugPrint2 "Liveness"
                            
                            let res = 
                                blocks
                                |> Map.toSeq
                                |> Seq.map (fun (name,b) -> name, (b,Blocks.liveness List.empty b))
                                |> Map.ofSeq

                            debugPrint "Blocks & Liveness" ignore res
                            let x = 
                                Seq.fold (fun s (_,b) -> 
                                    Blocks.registerAllocate 8 s b
                                ) Map.empty (Map.toSeq res)
                            debugPrint "Register Allocations" ignore x)
                    
                     
                    Sparc.writeMain stdout locfo insts
                else
                    IR.writeMain stdout locfo insts 
                
                exit 0
        | None -> 
            printfn "parse fail"
            exit 1