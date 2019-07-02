/// This module generates LLVM Assembler
module IR
open Utils
open Ast
open Ast'
open State
open Analyze
open Locator
open LLVM


let zipI32 a = Seq.zip (Seq.initInfinite (fun _ -> i32)) a

module Seq = 
    let addOne seq one =
        Seq.append seq (Seq.singleton one)

type 'a IRState = {
        Cnt:int
        Instructs: (Result * Instruction) seq
        Locs:'a
        EFrame: Result
        RegisterPrefix:string
    } 

module Generator =
    let empty locfo = { 
        Cnt = 0; 
        Instructs = Seq.empty; 
        Locs = locfo; 
        EFrame = Result.EFrame;
        RegisterPrefix = "r_";
    }
    let ofRegAndLoc registerName info = 
        { info with 
            RegisterPrefix = registerName }

    let emptyOfReg regName locfo cnt = 
        { ofRegAndLoc regName (empty locfo) with
            Cnt = cnt }

/// A Computational Expression Builder for emitting LLVM
type EmitterMonad() =
    inherit StateMonad()
    member this.Zero() = (Generator.empty ()),()
    member this.Yield(inst,x) = 
        fun state -> 
            { state with Instructs = Seq.addOne state.Instructs (x,inst) },x
    member this.ignore  = fun i -> (i,NoResult)
    member this.nextRegister =
        fun state -> ({ state with Cnt = state.Cnt + 1}, RegisterR <| sprintf "%s%i" state.RegisterPrefix state.Cnt) 
    member this.label =
        fun state -> ({ state with Cnt = state.Cnt + 1}, Label <| sprintf "label_%i" state.Cnt)
    member this.namedLabel name state =
        ({ state with Cnt = state.Cnt + 1}, Label <| sprintf "%s.label_%i" name state.Cnt)
    member this.findLocation name =
        fun state -> 
            state, state.Locs.VarLocations.Lookup name
    member this.findFloat f =
        fun state -> 
            state, state.Locs.FloatTable.[f]

    member this.findString s = 
        fun state ->
            state,state.Locs.StringTable.[s]

    member this.fieldID name = 
        fun state -> 
            (state,LiteralIntR state.Locs.FieldTable.[name])

    member this.floatRef f = 
        fun state -> 
            let (state,loc) = this.findFloat f state
            (state,GlobalR (sprintf "%s%i" VarNames.floatPrefix loc))
    member this.stringRef s = 
        fun state -> 
            let (state,loc) = this.findString s state
            (state,GlobalR (sprintf "%s%i" VarNames.stringPrefix loc))
    member this.enterScope scope = 
        fun state -> 
            let old = state.Locs.VarLocations
            ({ state with Locs = { state.Locs with VarLocations = scope } },old)
    member this.leaveScope scope =
        fun state -> 
            ({ state with Locs = { state.Locs with VarLocations = scope } },())
    member this.eframe state =
        state,state.EFrame 
    member this.setFrame newFrame state =
        { state with EFrame = newFrame },()
        

    member this.local loc = RegisterR (sprintf "loc_%i" loc)

let ir = EmitterMonad()  

let (=>) x y = x, y
let (~~~~) x = Comment x => NoResult

/// Emits the nesscary code to locate a variable ( to Load from or Store to )
let locateVar var =
    ir {
        let! loc = ir.findLocation var
        match loc with
        // Locals must always be in the current frame; hence the 0
        | (0,Local i) -> 
            yield ~~~~ (sprintf "Local %s" var)
            let local = ir.local i
            return local
        | (i, Local _) ->
            return failwithf "The local %s was not closed on!" var
        | (frame,Closure i) ->
            yield ~~~~ (sprintf "Frame %i -> Closure %s : %i" frame var i)
            let! eframe = ir.eframe
            do! ir.iter (fun i ->
                            ir {
                                let! ef = ir.eframe
                                let! newFramePtr = ir.nextRegister
                                yield GetElementPtr(LLVMType.EFrame.Ptr,ef,0) => newFramePtr
                                let! newFrame = ir.nextRegister
                                yield Load(LLVMType.EFrame.Ptr.Ptr,newFramePtr) => newFrame
                                do! ir.setFrame newFrame
                            }
                        ) (List.init frame id)
            let! lastFrame = ir.eframe
            let! locPtr = ir.nextRegister
            yield GetElementPtr2(LLVMType.EFrame.Ptr,lastFrame,1,i) => locPtr
            do! ir.setFrame eframe
            return locPtr
    }

let allocateLocal name =
    ir {
        let! loc = ir.findLocation name
        match loc with 
        | (_, Local _) ->
            let! local = locateVar name
            yield Alloca(i32) => local
        | (_, Closure _) ->
            return! locateVar name 
    }

let packArgs args = 
    ir {
        let! argsArrayReg = ir.nextRegister
        let aTy = LLVMType.array (List.length args) i32
        yield Alloca(aTy) => argsArrayReg
        do! ir.iter (fun (i,arg) ->
                ir {
                    let! argPtr = ir.nextRegister 
                    yield GetElementPtr(aTy.Ptr,argsArrayReg,i) => argPtr
                    yield Store(i32,arg,i32.Ptr,argPtr) => NoResult
                } 
            ) (Seq.zip (Seq.initInfinite id) args |> List.ofSeq)
        let! argsArrayPacked = ir.nextRegister
        yield BitCast(aTy.Ptr,argsArrayReg,LLVMType.zeroArrayI32.Ptr) => argsArrayPacked
    }

/// Converts an Ast to a sequence of LLVM IR
let rec toIr ast = 
    ir {
        match ast with 
        | IntConst   i -> return IntR i
        | BoolConst  b -> return BoolR (if b then BoolTypes.True else BoolTypes.False) 
        | StringConst s ->
            yield ~~~~ "Create string object"
            let! str = ir.nextRegister
            let sty = LLVMType.string s.Length
            yield Malloc(sty) => str
            let! stringLoc = ir.stringRef s
            let! storePtr = ir.nextRegister

            // Setup string
            yield ~~~~ "Set string"
            yield GetFieldPtr(sty.Ptr,str, FieldPostions.StringPtr) => storePtr
            yield Store(LLVMType.stringPtr s.Length,stringLoc,(LLVMType.stringPtr s.Length).Ptr,storePtr) => NoResult

            // Setup string length
            yield ~~~~ "Set string length"
            let! strLenPtr = ir.nextRegister
            yield GetFieldPtr(sty.Ptr,str, FieldPostions.StringLen) => strLenPtr
            yield Store(i32,LiteralIntR(s.Length),i32.Ptr,strLenPtr) => NoResult

            // Setup Object type
            yield ~~~~ "Set object type to string"
            let! objTyPtr = ir.nextRegister
            yield GetFieldPtr(sty.Ptr,str, FieldPostions.ObjectType) => objTyPtr
            yield Store(i32,ObjMaskR ObjectTypes.String,i32.Ptr,objTyPtr) => NoResult
            
            let! iPtr = ir.nextRegister
            yield PtrToInt(sty.Ptr ,str,i32) => iPtr
            let! oPtr = ir.nextRegister
            yield Or(i32,iPtr, MaskR PrimitiveTypes.Object) => oPtr
        | FloatConst f -> 
            yield ~~~~ "Create float pointer"
            let! iPtr = ir.nextRegister
            let! f = ir.floatRef f
            yield PtrToInt(Double.Ptr ,f,i32) => iPtr
            yield ~~~~ "Set type to float"
            let! fPtr = ir.nextRegister
            yield Or(i32,iPtr, MaskR PrimitiveTypes.Float) => fPtr
        | FunctionApplication(PrimitiveFunction (prim & (Primitive.Plus | Primitive.Minus | Primitive.Prod | Primitive.Div)),[lhs; rhs] & args) ->
            let! argregs = ir.map toIr args
            let! resultReg = ir.nextRegister
            yield CallPrim(prim,zipI32 argregs |> List.ofSeq) => resultReg
        | FunctionApplication(PrimitiveFunction prim,args) ->
            let! argregs = ir.map toIr args
            let! resultReg = ir.nextRegister
            yield CallPrim(prim,zipI32 argregs |> List.ofSeq) => resultReg
        | Sequence [] -> return NoResult
        | Sequence s ->
            let! res = ir.map toIr s
            return res |> List.rev |> List.head
        | Binding(name,value,body) ->
            yield ~~~~ ("Var: " + name)
            let! locptr = allocateLocal name 
            let! valueRes = toIr value
            yield Store(i32,valueRes,i32.Ptr,locptr) => NoResult
            return! toIr body
        | VariableRef var -> 
            if var = "this" then
                return Result.This
            else
                yield ~~~~ (sprintf "%s" var)
                let! location = locateVar var
                let! reg = ir.nextRegister
                yield Load(i32.Ptr,location) => reg
        | Mutate(var,exp) -> 
            yield ~~~~ (sprintf "Set var %s" var)            
            let! loc = locateVar var
            let! value = toIr exp
            yield Store(i32,value,i32.Ptr,loc) => NoResult
        | Return expr ->
            let! res = toIr expr
            yield Ret res => NoResult
        | MemberAccess(o,name) -> 
            yield ~~~~ (sprintf "Get field %s" name)
            let! obj = toIr o
            let! fieldValue = ir.nextRegister
            let! fieldID = ir.fieldID name
            yield GetField(obj,fieldID) => fieldValue
        | MemberBind(o,name,v) -> 
            yield ~~~~ (sprintf "Set field %s" name)
            let! obj = toIr o
            let! value = toIr v
            let! fieldID = ir.fieldID name
            yield SetField(obj, fieldID, value) => NoResult
        | If(tst,thn,els) -> 
            let! ifTrue = ir.label
            let! ifFalse = ir.label
            let! endIf = ir.label
            let! tst = toIr tst
            let! testResult = ir.nextRegister
            yield Icmp(Equal,tst,BoolTypes.True) => testResult
            yield BrIf(testResult,ifTrue,ifFalse) => NoResult
            yield MarkLabel ifTrue => NoResult
            let! thn = toIr thn
            yield Br endIf => NoResult
            yield MarkLabel ifFalse => NoResult
            let! els = toIr els
            yield Br endIf => NoResult
            yield MarkLabel endIf => NoResult
        | Scope(ast,scope,_) ->
            let! old = ir.enterScope scope
            let! res =  toIr ast
            do! ir.leaveScope old
            return res
        | While(tst,Scope(body,scope,cloCnt)) ->
            yield ~~~~ (sprintf "While Loop %i" cloCnt)
            let! whileTest = ir.namedLabel "whileTest"
            let! startWhile = ir.namedLabel "whileBody"
            let! endWhile = ir.namedLabel "endWhile"

            yield Br whileTest => NoResult
            
            yield MarkLabel whileTest => NoResult
            let! tstResult = toIr tst
            let! comp = ir.nextRegister
            yield Icmp(Equal,tstResult,BoolTypes.False) => comp
            yield BrIf(comp,endWhile,startWhile) => NoResult
            
            yield MarkLabel startWhile => NoResult
            let! eframe = ir.eframe
            let! old = ir.enterScope scope
            if cloCnt > 0 then
                let! framePtr = ir.nextRegister
                let! frame = ir.nextRegister
                let sizedFrame = LLVMType.SizedFrame cloCnt
                yield Malloc(sizedFrame) => framePtr
                yield BitCast(sizedFrame.Ptr,framePtr,LLVMType.EFrame.Ptr) => frame
                let! prevFramePtr = ir.nextRegister
                yield GetElementPtr(LLVMType.EFrame.Ptr,frame,0) => prevFramePtr
                yield Store(LLVMType.EFrame.Ptr,eframe,LLVMType.EFrame.Ptr.Ptr,prevFramePtr) => NoResult
                do! ir.setFrame frame
            else return ()
            
            let! _ = toIr body
            
            do! ir.setFrame eframe
            do! ir.leaveScope old
       
            yield Br whileTest => NoResult
            yield MarkLabel endWhile => NoResult
        | FunctionBindings(funs,body) -> 
            do! ir.iter (fun (name,f) ->
                    ir {
                        yield ~~~~ (sprintf "Function: %s" name)
                        let! local = allocateLocal name
                        let! cloReg = toIr f
                        yield Store(i32,cloReg,i32.Ptr,local) => NoResult
                    } 
                ) funs
            return! toIr body
        | Expression.Closure id -> 
            let! { Locs = { Functions = f } } = ir.GetState
            let cl = f.[id]
            let! cloReg = ir.nextRegister
            yield ~~~~ (sprintf "Closure ID: %i" id)
            yield Malloc(LLVMType.closure) => cloReg
            // Setup string
            yield ~~~~ "Set Function ID"
            let! cloFun = ir.nextRegister
            yield GetFieldPtr(LLVMType.closure.Ptr,cloReg, FieldPostions.ClosureFun) => cloFun
            yield Store(i32,LiteralIntR id,i32.Ptr,cloFun) => NoResult

            yield ~~~~ "Set Function's frame to current frame"
            let! cloFrame = ir.nextRegister
            let! eframe = ir.eframe
            yield GetFieldPtr(LLVMType.closure.Ptr,cloReg, FieldPostions.ClosureEnv) => cloFrame
            yield Store(LLVMType.EFrame.Ptr,eframe,LLVMType.EFrame.Ptr.Ptr,cloFrame) => NoResult

            // Setup Object type
            yield ~~~~ "Set object type to function"
            let! objTyPtr = ir.nextRegister
            yield GetFieldPtr(LLVMType.closure.Ptr,cloReg, FieldPostions.ObjectType) => objTyPtr
            yield Store(i32,ObjMaskR ObjectTypes.Closure,i32.Ptr,objTyPtr) => NoResult
            
            let! iPtr = ir.nextRegister
            yield PtrToInt(LLVMType.closure.Ptr, cloReg, i32) => iPtr
            let! oPtr = ir.nextRegister
            yield Or(i32,iPtr, MaskR PrimitiveTypes.Object) => oPtr
            return oPtr
        | FunctionApplication(func,args) -> 
            let! f = toIr func
            let! args = ir.map toIr args
            let! argsArrayPacked = packArgs args
            let! resReg = ir.nextRegister
            yield CallClo(f,args.Length, (LLVMType.zeroArrayI32.Ptr, argsArrayPacked)) => resReg
            return resReg
        | MethodCall(obj,name,args) -> 
            yield ~~~~ (sprintf "Method Call %s" name)
            let! obj = toIr obj
            let! fieldValue = ir.nextRegister
            let! fieldID = ir.fieldID name
            yield GetField(obj,fieldID) => fieldValue
            
            yield ~~~~ "Pack Args"
            let! argRegs = ir.map toIr args
            let! packedArgs = packArgs argRegs
            yield ~~~~ "Dispatch to method"
            let! result = ir.nextRegister
            yield CallMethod(fieldValue,obj,args.Length,(LLVMType.zeroArrayI32.Ptr, packedArgs)) => result
        | New(ctor,args) -> 
            yield ~~~~ "New"
            let! plainObjPtr = ir.nextRegister
            yield ~~~~ "Create Plain Object"
            yield Malloc(LLVMType.Plain) => plainObjPtr

            // Setup Object type
            yield ~~~~ "Set object type to plain"
            let! tyFieldPtr = ir.nextRegister
            yield GetFieldPtr(LLVMType.Plain.Ptr,plainObjPtr, FieldPostions.ObjectType) => tyFieldPtr
            yield Store(i32, ObjMaskR ObjectTypes.Plain, i32.Ptr, tyFieldPtr) => NoResult
            
            let! plainObj = ir.nextRegister
            yield PtrToInt(LLVMType.Plain.Ptr,plainObjPtr,i32) => plainObj
            let! oPtr = ir.nextRegister
            yield Or(i32,plainObj, MaskR PrimitiveTypes.Object) => oPtr
            yield ~~~~ "Get Ctor"
            let! ctor = toIr ctor

            yield ~~~~ "Set ctor field"
            yield SetField(oPtr,LiteralIntR 0,ctor) => NoResult

            yield ~~~~ "Pack Args"
            let! argRegs = ir.map toIr args
            let! packedArgs = packArgs argRegs
            yield ~~~~ "Dispatch to Ctor"
            yield CallMethod(ctor,oPtr,args.Length,(LLVMType.zeroArrayI32.Ptr, packedArgs)) => NoResult
            return oPtr
        | Func _ -> return failwith "Found a non lifted function!"
        | PrimitiveFunction _ -> return failwith "Found a primitive outside an application!"
        | While(_,_) -> return failwith "Found a while without a new scope!"
    }

let argsTos w args =
        args
        |> Seq.map (fun (x,y) -> x.ToString() + " " + y.ToString())
        |> String.concat ", "
        |> fprintf w "%s"

/// Writes an Result * LLVM Instruction pair to the given TextWriter
let rec writeIr writer ir  = 
    let write t = 
        fprintf writer "\t"
        fprintfn writer t

    let produceDispatchArgs f len args =
        seq { 
            yield (i32, f)
            yield (i32, LiteralIntR len)
            yield args
        }
    let produceDispatchMethodArgs ctor obj len args =
        seq { 
            yield (i32, ctor) 
            yield (i32, obj) 
            yield (i32, LiteralIntR len)
            yield args 
        }

    match ir with
    | Register reg, Call(name,args)                   -> write "%O = call i32 @%s(%a)" reg name argsTos args
    | Register reg, CallClo(f,len,args)               -> write "%O = call i32 @DispatchFunction(%a)" reg argsTos <| produceDispatchArgs f len args
    | Register reg, CallPrim(prim,args)               -> write "%O = call i32 @%A(%a)" reg prim argsTos args
    | Register reg, CallMethod(ctor,obj,len,args)     -> write "%O = call i32 @DispatchMethod(%a)" reg argsTos <| produceDispatchMethodArgs ctor obj len args
    | Register reg, BitCast(fromTy,from,toTy)         -> write "%O = bitcast %O %O to %O" reg fromTy from toTy
    | Register reg, PtrToInt(fromTy,from,toTy)        -> write "%O = ptrtoint %O %O to %O" reg fromTy from toTy
    | Register reg, Or(ty,lhs,rhs)                    -> write "%O = or %O %O, %O" reg ty lhs rhs
    | Register reg, Malloc(ty)                        -> write "%O = malloc %O" reg ty
    | Register reg, Alloca(ty)                        -> write "%O = alloca %O" reg ty
    | Register reg, Load(ty,from)                     -> write "%O = load %O %O" reg ty from
    | Register reg, GetElementPtr(ty,from,poc)        -> write "%O = getelementptr %O %O, i32 0, i32 %i " reg ty from poc
    | Register reg, GetElementPtr2(ty,from,field,poc) -> write "%O = getelementptr %O %O, i32 0, i32 %i, i32 %i " reg ty from field poc
    | Register reg, Icmp(cmp,res,shouldEqual)         -> write "%O = icmp %O i32 %O, %i" reg cmp res (int shouldEqual)
    | NoResult, Br(Label l)                           -> write "br label %%%s" l
    | NoResult, BrIf(reg,Label t,Label f)             -> write "br i1 %O, label %%%O, label %%%O" reg t f
    | NoResult, Comment c                             -> write "\n\t; %s" c
    | NoResult, Call(name,args)                       -> write "call i32 @%s(%a)" name argsTos args
    | NoResult, CallMethod(ctor,obj,len,args)         -> write "call i32 @DispatchMethod(%a)" argsTos <| produceDispatchMethodArgs ctor obj len args
    | NoResult, Ret reg                               -> write "ret i32 %O" reg
    | NoResult, Store(fromTy,from,stoTy,sto)          -> write "store %O %O, %O %O" fromTy from stoTy sto
    | NoResult, MarkLabel(Label l)                    -> fprintfn writer "%s:" l
    | _ -> failwithf "Invalid IR op: %A" ir

let writeIrSeq writer irSeq = Seq.iter (writeIr writer) irSeq

let writePrototypes writer primsSet =
    Set.iter (fun f -> 
        fprintfn writer "declare i32 @%A(%s)" f (Seq.init (Ast.Primitives.argCount f) (fun _ -> "i32") |> String.concat ", ")
    ) primsSet

(* ------------------------------------- 
    Debug Info
   ------------------------------------- *)
let debugMsgs = 
    [
        "beforeDis", "about to dispatch"
        "onDis", "dispatching "
    ] |> Map.ofList

let writeDebugCall writer msgName =
    let len = debugMsgs.[msgName].Length + 1
    fprintfn writer "\tcall i32 @puts(i8* getelementptr ([0 x i8]* bitcast ([%i x i8]* @.d.%s to [0 x i8]*), i32 0, i32 0))" len msgName   

let writeDebugInfo writer =
    let write t = fprintfn writer t
    write "declare i32 @puts(i8*)"
    debugMsgs
    |> Map.toSeq
    |> Seq.iter (fun (n,msg) ->
        let ty = LLVMType.stringArray msg.Length
        write "@.d.%s = private constant %O c\"%s\00\"" n ty msg
    )

(* -------------- End Debug Info ------------- *)

let writeFunction writer (id,(name,parameters,body,locationInfo)) =
    let frame = if locationInfo.ClosureCount > 0 then RegisterR "parentFrame" else Result.EFrame
    fprintfn writer "\ndefine i32 @%s_%i(%a) {\n" name id argsTos <| seq { yield (LLVMType.EFrame.Ptr, frame); yield! zipI32 (parameters|>Seq.map RegisterR) }
    if locationInfo.ClosureCount > 0 then
        fprintfn writer "\t%%myFrame = malloc { %%EFrame*, [%i x i32] }" locationInfo.ClosureCount
        fprintfn writer "\t%%eframe = bitcast { %%EFrame*, [%i x i32] }* %%myFrame to %%EFrame*" locationInfo.ClosureCount
        fprintfn writer "\t%%parentEFramePos = getelementptr %%EFrame* %%eframe, i32 0, i32 0"
        fprintfn writer "\tstore %%EFrame* %%parentFrame, %%EFrame** %%parentEFramePos"
    //FAKE THIS?!
    fprintfn writer "\t%%thisPtr = alloca %O" LLVMType.Plain
    fprintfn writer "\t%%thisI = ptrtoint %O %%thisPtr to i32" LLVMType.Plain.Ptr
    fprintfn writer "\t%%this = or i32 %%thisI, %O" (int PrimitiveTypes.Object )
    let ({ Instructs = ins },_) = toIr body <| Generator.empty locationInfo
    seq {
        yield! parameters
            |> Seq.mapi (fun i arg ->
                let ({ Instructs = i },res) = allocateLocal arg (Generator.emptyOfReg ".argSetup" locationInfo i)
                seq {
                    yield! i
                    yield (NoResult, Store(i32,RegisterR arg, i32.Ptr, res))
                }
        ) |> Seq.concat
        yield! ins
        yield (NoResult,Ret (BoolR BoolTypes.Void))
    } |> writeIrSeq writer
    fprintfn writer "}"

let writeMethod writer (id,(name,parameters,body,locationInfo)) =
    let frame = if locationInfo.ClosureCount > 0 then RegisterR "parentFrame" else Result.EFrame
    fprintfn writer "\ndefine i32 @%s.m_%i(%a) {\n" name id argsTos <| seq { yield (LLVMType.EFrame.Ptr, frame); yield (i32, RegisterR "this"); yield! zipI32 (parameters|>Seq.map RegisterR) }
    if locationInfo.ClosureCount > 0 then
        fprintfn writer "\t%%myFrame = malloc { %%EFrame*, [%i x i32] }" locationInfo.ClosureCount
        fprintfn writer "\t%%eframe = bitcast { %%EFrame*, [%i x i32] }* %%myFrame to %%EFrame*" locationInfo.ClosureCount
        fprintfn writer "\t%%parentEFramePos = getelementptr %%EFrame* %%eframe, i32 0, i32 0"
        fprintfn writer "\tstore %%EFrame* %%parentFrame, %%EFrame** %%parentEFramePos"
    let ({ Instructs = ins },_) = toIr body <| Generator.empty locationInfo
    seq {
        yield! parameters
            |> Seq.mapi (fun i arg ->
                let ({ Instructs = i },res) = allocateLocal arg (Generator.emptyOfReg ".argSetup" locationInfo i)
                seq {
                    yield! i
                    yield (NoResult, Store(i32,RegisterR arg, i32.Ptr, res))
                }
        ) |> Seq.concat
        yield! ins
        yield (NoResult,Ret (BoolR BoolTypes.Void))
    } |> writeIrSeq writer
    fprintfn writer "}"

let writeFunctions writer = 
    Seq.iter (fun f -> 
                writeFunction writer f
                writeMethod writer f)


let createDispatchParams writer id parameters = 
//    let paramlen = List.length parameters
//    fprintfn writer "\t%%args%i = bitcast [ 0 x i32 ]* %%params to [ %i x i32 ]*" id paramlen
    List.mapi (fun i param -> 
//        fprintfn writer "\t%%%s%i_%i_ptr = getelementptr [ %i x i32 ]* %%args%i, i32 0, i32 %i" param id i paramlen id i
        fprintfn writer "\t%%%s%i_%i_ptr = getelementptr [ 0 x i32 ]* %%params, i32 0, i32 %i" param id i i
        fprintfn writer "\t%%%s%i_%i = load i32* %%%s%i_%i_ptr" param id i param id i
        RegisterR <| sprintf "%s%i_%i" param id i
    ) parameters


let createDispatchSwitch writer functions =
    Seq.iter (fst >> (fun id -> 
        fprintfn writer "\t    i32 %i, label %%call_%i" id id
    )) functions

let createFunctionDispatchCases writer includeThis functions =
    Seq.iter (fun (id,(name,parameters,_,_)) ->
        fprintfn writer "call_%i:" id
        let l = createDispatchParams writer id parameters
        let argSeq = seq { 
            yield LLVMType.EFrame.Ptr, Result.EFrame
            if includeThis then
                yield i32, Result.This
            yield! zipI32 l 
        }

        fprintfn writer "\t%%res_%i = call i32 @%s_%i(%a)" id name id argsTos argSeq
        fprintfn writer "\tret i32 %%res_%i" id
    ) functions

let writeDispatchFunction writer functions =
    fprintfn writer "define i32 @DispatchFunction(i32 %%functionClo, i32 %%paramLen, %O %%params){" LLVMType.zeroArrayI32.Ptr
    fprintfn writer "\t%%cloPtr = and i32 %%functionClo, %i" ~~~(int PrimitiveTypes.Object)
    fprintfn writer "\t%%cloI = inttoptr i32 %%cloPtr to i32*"
    fprintfn writer "\t%%clo = bitcast i32* %%cloI to %%Closure*"
    fprintfn writer "\t%%eframePtr = getelementptr %%Closure* %%clo, i32 0, i32 3"
    fprintfn writer "\t%%eframe = load %%EFrame** %%eframePtr"
    fprintfn writer "\t%%functionIdPtr = getelementptr %%Closure* %%clo, i32 0, i32 2"
    fprintfn writer "\t%%functionId = load i32* %%functionIdPtr"
    fprintfn writer "\tswitch i32 %%functionId, label %%NotFound ["
    createDispatchSwitch writer functions
    fprintfn writer "\t]"
    createFunctionDispatchCases writer false functions
    fprintfn writer "NotFound:"
    fprintfn writer "\tret i32 0"
    fprintfn writer "}" 

let writeDispatchMethod writer functions =
    let functions = Seq.map (fun (id,(name,ps,x,y)) -> (id,(name + ".m",ps,x,y))) functions
    fprintfn writer "define i32 @DispatchMethod(i32 %%ctorClo,i32 %O, i32 %%paramLen, %O %%params){" Result.This LLVMType.zeroArrayI32.Ptr
    fprintfn writer "\t%%cloPtr = and i32 %%ctorClo, %i" ~~~(int PrimitiveTypes.Object)
    fprintfn writer "\t%%cloI = inttoptr i32 %%cloPtr to i32*"
    fprintfn writer "\t%%clo = bitcast i32* %%cloI to %%Closure*"
    fprintfn writer "\t%%eframePtr = getelementptr %%Closure* %%clo, i32 0, i32 3"
    fprintfn writer "\t%%eframe = load %%EFrame** %%eframePtr"
    fprintfn writer "\t%%functionIdPtr = getelementptr %%Closure* %%clo, i32 0, i32 2"
    fprintfn writer "\t%%functionId = load i32* %%functionIdPtr"
    fprintfn writer "\tswitch i32 %%functionId, label %%NotFound ["
    createDispatchSwitch writer functions
    fprintfn writer "\t]"
    createFunctionDispatchCases writer true functions
    fprintfn writer "NotFound:"
    fprintfn writer "\tret i32 0"
    fprintfn writer "}"


let writeMain writer locInfo irSeq = 
    let write t = fprintfn writer t

    write "target triple = \"i686-pc-linux-gnu\"\n"

    write "%O = type { %O, [0 x i32] }" LLVMType.EFrame LLVMType.EFrame.Ptr
    write "@EmptyFrame = external constant %O" LLVMType.EFrame
    write "%%Field = type { i32, i32, %%Field* }"
    write "@EmptyField = external constant %%Field"
    write "%O = type %O\n\n" LLVMType.closure LLVMType.closureStruct 

    locInfo.StringTable
    |> Map.toSeq
    |> Seq.sortBy snd
    |> Seq.iter (fun (s,i) ->
        fprintfn writer "%s%i = internal constant %O c\"%s\\00\"" VarNames.globalStringPrefix i (LLVMType.stringArray s.Length) (s.Replace("\n","\\0A"))
    )
    
//    writeDebugInfo writer
    
    writer.WriteLine()

    locInfo.FloatTable
    |> Map.toSeq
    |> Seq.sortBy snd
    |> Seq.iter (fun (f,i) ->
        fprintfn writer "%s%i = internal constant double %.18E" VarNames.globalFloatPrefix i f
    )

    writer.WriteLine()

    if locInfo.FieldTable.Count > 0 then
        write "declare i32 @GetField(i32, i32)"
        write "declare i32 @SetField(i32, i32, i32)"

    writePrototypes writer locInfo.PrimitivesUsed
    writer.WriteLine()

    locInfo.Functions 
    |> Map.toSeq
    |> writeFunctions writer

    writer.WriteLine()

    locInfo.Functions
    |> Map.toSeq
    |> fun funcs ->
        writeDispatchFunction writer funcs
        writeDispatchMethod   writer funcs

    writer.WriteLine()

    let frame = if locInfo.ClosureCount > 0 then RegisterR "parentFrame" else Result.EFrame
    write "define i32 @AppMain(%O %O) {" LLVMType.EFrame.Ptr frame
    if locInfo.ClosureCount > 0 then
        fprintfn writer "\t%%myFrame = malloc { %%EFrame*, [%i x i32] }" locInfo.ClosureCount
        fprintfn writer "\t%%eframe = bitcast { %%EFrame*, [%i x i32] }* %%myFrame to %%EFrame*" locInfo.ClosureCount
        fprintfn writer "\t%%parentEFramePos = getelementptr %%EFrame* %%eframe, i32 0, i32 0"
        fprintfn writer "\tstore %%EFrame* %%parentFrame, %%EFrame** %%parentEFramePos"
    writeIrSeq writer (Seq.addOne irSeq (NoResult,Ret (IntR 0)))
    write "}"
 
    sprintf @"
define i32 @main() {
    call i32 @AppMain(%O @EmptyFrame)
    ret i32 0
}"   LLVMType.EFrame.Ptr
    |> write "%s"

(*
type SmallAst = 
    | IntLit of int
    | Sum of SmallAst * SmallAst

type In = 
    | LoadInt of int
    | Add of In * In

type ParaBindF<'a,'b> = int -> int * seq<'a> * 'b

type ParaBind() = 
    member this.Bind((a:ParaBindF<_,_>,b:ParaBindF<_,_>),f) =
        fun i -> 
            [ async { return a (i*2) }; async { return b (i*2+1) } ]
            |> Async.Parallel
            |> Async.RunSynchronously
            |> function 
                | [|(anewstate,acode,ares); (bnewstate,bcode,bres)|] ->
                    let (nextI,code,res) = f (ares,bres) bnewstate
                    (nextI, Seq.append acode (Seq.append bcode code), res)
                | _ -> failwith "bad result"

    member this.Return(r) =
        fun i -> (i,Seq.empty,r)

    member this.Bind(a,f) =
        fun i ->
            let (i,r) = a (i*2)
            f r (i*2+1)


let para = ParaBind()



let rec f a =
    para {
        match a with
        | IntLit i ->
            return i
        | Sum(l,r) ->
            let! (lres,rres) = (f l, f r)
            return lres + rres

    }
*)
