module Blocks


open LLVM
open IR
open System.Collections.Generic

type Block = {
        mutable Name : string
        mutable ToBlocks : Set<string>
        IR : (Result * Instruction) list
    }
    with
    static member empty() = {
            Name = null
            ToBlocks = Set.empty
            IR = List.empty
        }

module Seq =
    let cons head tail =
        seq {
            yield head
            yield! tail
        }
module Map = 
    let ofDict (dict:Dictionary<_,_>) = dict |> Seq.map (fun kv -> kv.Key,kv.Value) |> Map.ofSeq

/// Turns a sequence of LLVM Instructions into a series of basic blocks
/// Returns a map of basic blocks and the first block
let blockify name irSeq =
    let blocks = Dictionary()
    let block = ref <| Block.empty()
    irSeq
    |> List.ofSeq
    |> List.rev
    |> List.iter (fun ((_,ins) as insPair)  -> 
        match ins with 
        | MarkLabel(Label label) -> //end block
            block.Value.Name <- label
            blocks.Add(label,block.Value)
        | Br(Label label) -> 
            block := Block.empty()
            block.Value.ToBlocks <- Set.add label block.Value.ToBlocks
        | BrIf(cond,Label ifTrue,Label ifFalse) -> 
            block := Block.empty()
            block.Value.ToBlocks <- block.Value.ToBlocks |> (Set.add ifFalse >> Set.add ifTrue)
        | _ -> ()
        block := { !block with IR = insPair :: (!block).IR }
    )
    let blocks = Map.ofDict blocks
    block.Value.Name <- name
    Map.add name !block blocks, !block

/// Produces a Liveness graph (a register and its edges ) and the the registers live on exit of a basic block
let liveness allDefs { IR = body } =
    let allDefs = 
        body 
        |> List.map fst 
        |> List.fold (fun state -> function Reg reg -> reg :: state | _ -> state ) List.empty 
        |> List.append allDefs

    (body,(Set.empty,Set.empty))
    ||> List.foldBack (fun (result,ins) (edges,onExit) ->        
        let uses =
            let uses = Set.empty
            match ins with 
            | Store(_, Reg reg, _, Reg reg2) | Or(_, Reg reg, Reg reg2) ->
                uses
                |> Set.add reg 
                |> Set.add reg2
            | BrIf(Reg reg,_,_)
            | Ret(Reg reg)               | BitCast(_, Reg reg, _) 
            | Icmp(_,Reg reg,_)          | Load(_,Reg reg)
            | Or(_,Reg reg,_)            | PtrToInt(_, Reg reg, _)
            | GetElementPtr(_,Reg reg,_) | GetElementPtr2(_,Reg reg,_,_)
            | Store(_, Reg reg, _, _)    | Store(_, _, _, Reg reg) ->
                Set.add reg uses
            | CallPrim(_,args) | Call(_,args) -> 
                List.fold (fun uses -> snd >> (function Reg reg -> Set.add reg uses | _ -> uses)) uses args
            | CallMethod(Reg reg, Reg reg2, _, (_, Reg arg)) ->
                uses
                |> Set.add reg2
                |> Set.add reg 
                |> Set.add arg
            | CallClo(Reg reg, _ , (_, Reg arg)) -> 
                uses
                |> Set.add reg
                |> Set.add arg
            | _ -> uses

        let defs = match result with Reg reg -> Some reg | _ -> None

        // (live on exit) = (live on exit from the one before - definitions) U uses
        let exitSubDefs =
            match defs with
            | Some reg -> Set.remove reg onExit
            | None     -> onExit
 
        let exit = Set.union exitSubDefs uses

        // newEdges       = {(x,y) | x in definitions, y in (live on exit from the one before - definitions)}
        let newEdges = 
            match defs with 
            | Some x -> 
                exitSubDefs
                |> Set.map (fun y -> (x,y))
                |> Set.union edges
            | None -> edges
        (newEdges,exit)
    )
    ||> (fun edges out ->
        let edges =
            allDefs
            |> Seq.collect (fun def -> Seq.map (fun o -> def,o) out)
            |> Set.ofSeq
            |> Set.union edges
        let edgeGroups = 
            edges 
            |> Seq.groupBy fst 
            |> Seq.toList
        edgeGroups,out
    )


/// This is where the magic happens
/// It takes the maxium registers available, a map containing the known register mappings and a sequence of edges (grouped together by their shared start)
/// And maps the virtual registers to acctual machine registers
let registerAllocate maxRegs registers (_,(group,_)) =
    let allRegisters = [0 .. maxRegs-1] |> Set.ofList
    let nextRegister = Set.minElement
    group
    |> Seq.sortBy (snd >> Seq.length >> (~-))
    |> Seq.fold (fun registers (reg,edges) ->
        let availableRegisters, registers = 
            Seq.fold (fun (regChoices,registers) (_,edge) -> 
                if Map.containsKey edge registers then
                    (Set.remove (Map.find edge registers) regChoices, registers)
                else
                    let next = nextRegister regChoices
                    (Set.remove next regChoices, Map.add edge next registers)            
            ) (allRegisters,registers) edges
        Map.add reg (nextRegister availableRegisters) registers
    ) registers