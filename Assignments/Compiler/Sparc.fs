/// Emits sparc assembler
module Sparc
open LLVM
open IR
open Locator
open Blocks

module SparcNames = 
    let localPrefix = "%l"
    let globalPrefix = ""

type SparcResult =
    | IntR of int
    | FloatR of float
    | BoolR of BoolTypes
    | LiteralIntR of int
    | MaskR of PrimitiveTypes
    | ObjMaskR of ObjectTypes
    | RegisterR of int
    | GlobalRegisterR of int
    | GlobalR of string
    | NoSparcResult
    override this.ToString() =
        match this with
        | IntR i -> string (i*4)
        | FloatR f -> f.ToString("E")
        | LiteralIntR ty -> string ty
        | MaskR ty -> string (int ty)
        | ObjMaskR ty -> string (int ty)
        | BoolR ty -> string (int ty)
        | RegisterR reg -> SparcNames.localPrefix + (string reg)
        | GlobalRegisterR reg -> "%g" + (string reg)
        | GlobalR reg -> SparcNames.globalPrefix + reg
        | NoSparcResult -> "<NOSPRACRESUULT>"
    static member FromResult registerMap result = 
        match result with 
        | Result.IntR i   -> IntR i
        | Result.FloatR f -> FloatR f
        | Result.BoolR b  -> BoolR b
        | Result.LiteralIntR lit -> LiteralIntR lit
        | Result.MaskR t -> MaskR t
        | Result.ObjMaskR obj -> ObjMaskR obj 
        | Result.RegisterR reg -> 
            if Map.containsKey reg registerMap then
                RegisterR <| Map.find reg registerMap
            else failwithf "Register allocation not found!!! %s" reg
        | Result.GlobalR g -> GlobalR g
        | Result.NoResult -> NoSparcResult
        | GetElementPtr2R(_,_,_,_) 
        | GetElementPtrR(_,_,_) -> failwith "hah, not in sparc"

let (|SparcReg|_|) registerMap =
    function 
    | Result.RegisterR reg as r -> 
        if Map.containsKey reg registerMap
        then Some(SparcResult.FromResult registerMap r)
        else Some(GlobalRegisterR 1)
    | _ -> None


let toSparc writer irSeq registerMap = 
    let (|SparcReg|_|) = (|SparcReg|_|) registerMap
    let (|Sparc|_|) = Some << (function SparcReg reg -> reg | a -> SparcResult.FromResult registerMap a)
    let convert (Sparc x) = x
    let writeNT  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNT t
    irSeq
    |> Seq.iter (fun ir -> 
        match ir with
        | SparcReg reg, Call(name,args) ->
            List.iteri (fun i (ty,register) -> 
                let result = convert register
                match result with
                | GlobalRegisterR reg 
                | RegisterR reg -> 
                    write "mov %O, %%o%i" result i
                | _ -> write "set %O, %%o%i" result i
            ) args
            write "call %s" name
            write "nop"
            write "mov %%o0, %O" reg
        | SparcReg reg, CallClo(f,len,(_,Sparc arg)) -> 
            match arg with
            | GlobalRegisterR reg 
            | RegisterR reg -> 
                write "mov %O, %%o0" arg
            | _ -> write "set %O, %%o0" arg
            write "call DispatchFunction" 
            write "nop"
            write "mov %%o0, %O" reg
            //reg argsTos <| seq { yield (i32, f); yield (i32, LiteralIntR len); yield args }
        | SparcReg reg, CallPrim(prim,args) -> 
            List.iteri (fun i (ty,register) -> 
                let result = convert register
                match result with
                | GlobalRegisterR reg 
                | RegisterR reg -> 
                    write "mov %O, %%o%i" result i
                | _ -> write "set %O, %%o%i" result i
            ) args
            write "call %A" prim
            write "nop"
            write "mov %%o0, %O" reg
        | SparcReg reg, CallMethod(ctor,obj,len,(_,Sparc arg)) -> 
            match arg with
            | GlobalRegisterR reg 
            | RegisterR reg -> 
                write "mov %O, %%o0" arg
            | _ -> write "set %O, %%o0" arg
            write "call DispatchMethod" 
            write "nop"
            write "mov %%o0, %O" reg
//            write "%O = call i32 @DispatchMethod(%a)" reg argsTos <| seq { yield (i32, ctor); yield (i32, obj); yield (i32, LiteralIntR len); yield args }
        | SparcReg reg, Or(ty,Sparc lhs,Sparc rhs) ->
            write "or %O, %O, %O" lhs rhs reg
        | SparcReg reg, Malloc(ty) ->
            write "set %i, %%o0" ty.Size 
            write "call malloc"
            write "nop"
            write "mov %%o0, %O" reg
        | SparcReg reg, Alloca(ty) ->
            write "sub %%sp, %i, %%sp" ty.Size
            write "mov %%sp, %O" reg
        | SparcReg reg, Load(_,Sparc from) ->
            write "ld [%O], %O" from reg
        | SparcReg reg, GetElementPtr(_,Sparc from,poc) ->
            write "add %O, %i, %O " from (4*poc) reg
        | SparcReg reg, GetElementPtr2(_,Sparc from,field,poc) -> 
            write "add %O, %i, %O" from (field * 4) reg
            write "add %O, %i, %O " reg (poc * 4) reg
        | SparcReg reg, Icmp(cmp,Sparc res,shouldEqual) -> 
            write "mov %O, %%o7" res
            write "subcc %%o7, %i, %%g0" (int shouldEqual)
        | SparcReg reg, BitCast(_,Sparc from,_)
        | SparcReg reg, PtrToInt(_,Sparc from,_) ->
            match from with
            | GlobalR _ -> write "set %O, %O" from reg 
            | _ -> write "or %%g0, %O, %O" from reg            
        | NoResult, Br(Label l) ->
            write "ba .%s" l
            write "nop !delay"
        | NoResult, BrIf(reg,Label t,Label f) -> 
            write "be .%s !%O" t reg
            write "nop !delay"
            write "ba .%s" f 
            write "nop !delay"
        | NoResult, Comment c -> 
            write "\n\t! %s" c
        | NoResult, Call(name,args) ->
            List.iteri (fun i (_,register) -> 
                let result = convert register
                match result with
                | GlobalRegisterR reg 
                | RegisterR reg -> 
                    write "mov %O, %%o%i" result i
                | _ -> write "set %O, %%o%i" result i
            ) args 
            write "call %s" name
            write "nop"
//            write "call i32 @%s(%a)" name argsTos args
        | NoResult, CallMethod(ctor,obj,len,(_,Sparc arg)) -> 
            write "set %O, %%o0" arg
            write "call DispatchMethod" 
            write "nop"
        | NoResult, Ret reg ->
            write "set %O, %%i0" reg 
            write "retl"
            write "nop"
        | NoResult, Store(_,Sparc from,_,Sparc sto) -> 
//            match from with
//            | RegisterR _ ->  write "mov %O, %O" from sto
//            | _ ->  write "set %O, %O" from sto
            match from with
            | GlobalRegisterR _ 
            | RegisterR _ -> 
              write "st %O, [%O]" from sto
            | _ ->
                write "set %O,%%o7" from
                write "st %%o7, [%O]" sto
        | NoResult, MarkLabel(Label l) -> 
            write ".%s:" l
        | _ -> failwithf "Invalid IR op: %A" ir
    )


let writeFunction writer (id,(name,parameters,body,locationInfo)) =
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t

    writer.WriteLine()

    write ".align 16"
    write ".global %s_%i" name id
    write ".type %s_%i #function" name id
    writeNL "%s_%i:" name id

    if locationInfo.ClosureCount > 0 then
        write "set %i, %%o0" locationInfo.ClosureCount
        write "call malloc"
        write "nop" 
        write "st %%i0, [%%o0]"

    //    //FAKE THIS?!
//    fprintfn writer "\t%%thisPtr = alloca %O" LLVMType.Plain
//    fprintfn writer "\t%%thisI = ptrtoint %O %%thisPtr to i32" LLVMType.Plain.Ptr
//    fprintfn writer "\t%%this = or i32 %%thisI, %O" (int PrimitiveTypes.Object )
    
    //    seq {
//        yield! parameters
//            |> Seq.mapi (fun i arg ->
//                let ({ Instructs = i },res) = allocateLocal arg (Generator.emptyOfReg ".argSetup" locationInfo i)
//                seq {
//                    yield! i
//                    yield (NoResult, Store(i32,Register arg, i32.Ptr, res))
//                }
//        ) |> Seq.concat

    let ({ Instructs = ins },_) = IR.toIr body <| Generator.empty locationInfo

    let registerMap = 
        blockify name ins
        |> fst
        |> Map.map (fun name b -> (b,Blocks.liveness List.empty b))
        |> Map.fold (fun s _ b -> Blocks.registerAllocate 8 s b) Map.empty

    if Map.containsKey "eframe" registerMap then
        write "mov %%o0, %%l%i" (Map.find "eframe" registerMap)

    toSparc writer ins registerMap

    write "set %i, %%i0" (int BoolTypes.Void)
    write "restore"
    write "retl"

let writeMethod writer (id,(name,parameters,body,locationInfo)) =
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t
    
    writer.WriteLine()

    write ".align 16"
    write ".global %s.m_%i" name id
    write ".type %s.m_%i #function" name id
    writeNL "%s.m_%i:" name id

    if locationInfo.ClosureCount > 0 then
        write "set %i, %%o0" locationInfo.ClosureCount
        write "call malloc"
        write "nop" 
        write "st %%i0, [%%o0]"

    
    //    seq {
//        yield! parameters
//            |> Seq.mapi (fun i arg ->
//                let ({ Instructs = i },res) = allocateLocal arg (Generator.emptyOfReg ".argSetup" locationInfo i)
//                seq {
//                    yield! i
//                    yield (NoResult, Store(i32,Register arg, i32.Ptr, res))
//                }
//        ) |> Seq.concat

    let ({ Instructs = ins },_) = IR.toIr body <| Generator.empty locationInfo

    let registerMap = 
        blockify name ins
        |> fst
        |> Map.map (fun name b -> (b,Blocks.liveness List.empty b))
        |> Map.fold (fun s _ b -> Blocks.registerAllocate 8 s b) Map.empty

    if Map.containsKey "eframe" registerMap then
        write "mov %%o0, %%l%i" (Map.find "eframe" registerMap)

    toSparc writer ins registerMap

    write "set %i, %%i0" (int BoolTypes.Void)
    write "restore"
    write "retl"

let writeFunctions writer = 
    Seq.iter (fun f -> 
                writeFunction writer f
                writeMethod writer f)


let createDispatchParams writer id parameters includeThis = 
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t
    List.iteri (fun i param -> 
        write "ld [%%l1 + %i], %%o%i" (i*4) (i+if includeThis then 2 else 1)
    ) parameters


let createDispatchSwitch labelName writer functions =
    Seq.iter (fst >> (fun id -> 
        fprintfn writer "\tsubcc %%l0, %i, %%g0" id
        fprintfn writer "\tbz .%s_%i" labelName id
        fprintfn writer "\tnop"
    )) functions

let createFunctionDispatchCases labelName writer includeThis functions =
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t

    Seq.iter (fun (id,(name,parameters,_,_)) ->
        writeNL ".%s_%i:" labelName id
        createDispatchParams writer id parameters includeThis
        write "call %s_%i" name id
        write "nop"
        write "mov %%o0, %%i0" 
        write "restore"
        write "retl"
        write "nop"
    ) functions

let writeDispatchFunction writer functions =
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t

    write ".align 16"
    write ".global DispatchFunction"
    write ".type DispatchFunction #function"
    
    //!(i32 %%ctorClo,i32 %O, i32 %%paramLen, %O %%params){ Result.This LLVMType.zeroArrayI32.Ptr
    // %i0 = clo
    // %i1 = paramLen
    // %i2 = params 
    writeNL "DispatchFunction:"
    write "save %%sp, -96, %%sp"
    write "and %%i0, %i, %%i0" ~~~(int PrimitiveTypes.Object)
    write "!eframe"
    write "ld [%%i0 + 12], %%o0"
    write "!function id"
    write "ld [%%i0 + 8], %%l0"
    write "! parameters"
    write "mov %%i2, %%l1"
    createDispatchSwitch "call" writer functions
    write "ba .FunNotFound"
    write "nop"
    createFunctionDispatchCases "call" writer true functions
    writeNL ".FunNotFound:"
    write "set %i, %%i0" (int BoolTypes.Void)
    write "restore"
    write "retl"
    write "nop"

let writeDispatchMethod writer functions =
    let functions = Seq.map (fun (id,(name,ps,x,y)) -> (id,(name + ".m",ps,x,y))) functions
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t

    write ".align 16"
    write ".global DispatchMethod"
    write ".type DispatchMethod #function"

    //!(i32 %%ctorClo,i32 %O, i32 %%paramLen, %O %%params){ Result.This LLVMType.zeroArrayI32.Ptr
    // %i0 = clo
    // %i1 = this
    // %i2 = paramLen
    // %i3 = params 
    writeNL "DispatchMethod:"
    write "save %%sp, -96, %%sp"
    write "and %%i0, %i, %%i0" ~~~(int PrimitiveTypes.Object)
    write "! eframe"
    write "ld [%%i0 + 12], %%o0"
    write "mov %%i0, %%o0"
    write "! function id"
    write "ld [%%i0 + 8], %%l0"
    write "! parameters"
    write "mov %%i3, %%l1"
    
    createDispatchSwitch "mcall" writer functions
    write "ba .MethodNotFound"
    write "nop"
    createFunctionDispatchCases "mcall" writer true functions
    writeNL ".MethodNotFound:"
    write "restore"
    write "set %i, %%i0" (int BoolTypes.Void)
    write "retl"
    write "nop"

let writeROData writer locInfo = 
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t

    writer.WriteLine()
    write ".section \".rodata\""
    write ".align	2"
    write "! String Table"
    locInfo.StringTable
    |> Map.toSeq
    |> Seq.sortBy snd
    |> Seq.iter (fun (s,i) ->
        writeNL ".str_%i: .asciz \"%s\"" i (s.Replace("'",@"\'").Replace("\n","\\n"))
    )
    
//    writeDebugInfo writer
    
    writer.WriteLine()

    write ".align	4"
    write "! Float Table"
    locInfo.FloatTable
    |> Map.toSeq
    |> Seq.sortBy snd
    |> Seq.iter (fun (f,i) ->
        writeNL ".f_%i: .double %.18E" i f
    )

let writeMain writer locInfo irSeq = 
    let writeNL  t = fprintfn writer t
    let write t = fprintf writer "\t"; writeNL t

    write ".file \"awesomeness\""

    writeROData writer locInfo

    writer.WriteLine()

    write ".section \".text\""

    locInfo.Functions 
    |> Map.toSeq
    |> writeFunctions writer

    writer.WriteLine()

    locInfo.Functions
    |> Map.toSeq
    |> fun funcs ->
        writer.WriteLine()
        writeDispatchFunction writer funcs
        writer.WriteLine()
        writer.WriteLine()
        writeDispatchMethod   writer funcs
        writer.WriteLine()

    writer.WriteLine()

    write ".align 16"
    write ".global AppMain"
    write ".type AppMain #function"
    writeNL "AppMain:"
    write "save %%sp, -96, %%sp"
    if locInfo.ClosureCount > 0 then
        write "set %i, %%o0" (LLVMType.SizedFrame locInfo.ClosureCount).Size
        write "call malloc"
        write "nop" 
        write "st %%i0, [%%o0]"
//    let (blocks,firstBlock) = blockify "main" irSeq
//    let res = 
////        blocks
//        blockify "main" irSeq
//        |> fst
//        |> Map.map (fun name b -> (b,Blocks.liveness List.empty b))

    let registerMap =
        blockify "main" irSeq
        |> fst
        |> Map.map (fun name b -> (b,Blocks.liveness List.empty b))
        |> Map.fold (fun s _ b -> Blocks.registerAllocate 8 s b) Map.empty

    if Map.containsKey "eframe" registerMap then
        write "mov %%o0, %%l%i" (Map.find "eframe" registerMap)

    toSparc writer irSeq registerMap

    write "set 0,%%i0"
    write "restore"
    write "retl"
    write "nop"


    write ""
    write ".align 16"
    write ".global main"
    write ".type main #function"
    writeNL "main:"
    write "save %%sp, -96, %%sp"
    write "set EmptyFrame, %%o0"
    write "call AppMain"
    write "nop"
    write "set 0, %%i0"
    write "restore"
    write "retl"
    write "nop"

    