module LLVM

/// Some basic LLVM Constants
module VarNames = 
    let globalPrefix = "@"
    let floatPrefix = ".f_"
    let stringPrefix = ".str_"
    let globalFloatPrefix = globalPrefix + floatPrefix
    let globalStringPrefix = globalPrefix + stringPrefix
    let localPrefix = "%"

/// Represents the possible primitive types of the SOURCE language
type PrimitiveTypes =
    | Int    = 0b00
    | Float  = 0b01
    | Object = 0b10
    | Bool   = 0b11
/// Represents the possible object types of the SOURCE language 
and ObjectTypes =
    | Plain   = 0b00
    | Closure = 0b01
    | String  = 0b10
/// An enum representing the position of object fields in the target language
and FieldPostions = 
    | ObjectType = 0
    | Fields = 1
    | StringLen = 2
    | StringPtr = 3
    | ClosureFun = 2
    | ClosureEnv = 3
/// Constant boolean values for the SOURCE language
and BoolTypes = 
    | Void =  0b1011
    | True =  0b0111
    | False = 0b0011

/// Possible results of evaluating an LLVM expression
type Result =
    | IntR of int
    | FloatR of float
    | BoolR of BoolTypes
    | LiteralIntR of int
    | MaskR of PrimitiveTypes
    | ObjMaskR of ObjectTypes
    | GetElementPtrR of LLVMType * Result * int
    | GetElementPtr2R of LLVMType * Result * int * int
    | RegisterR of string
    | GlobalR of string
    | NoResult
    override this.ToString() =
        match this with
        | IntR i -> string (i*4)
        | FloatR f -> f.ToString("E")
        | LiteralIntR ty -> string ty
        | MaskR ty -> string (int ty)
        | ObjMaskR ty -> string (int ty)
        | BoolR ty -> string (int ty)
        | RegisterR reg -> VarNames.localPrefix + reg
        | GlobalR reg -> VarNames.globalPrefix + reg
        | GetElementPtrR(ty,reg,i) -> sprintf "getelementptr (%O %O, i32 0, i32 %i)" ty reg i
        | GetElementPtr2R(ty,reg,i,i2) -> sprintf "getelementptr (%O %O, i32 0, i32 %i, i32 %i)" ty reg i i2
        | NoResult -> "<NORESUULT>"
    static member EFrame = RegisterR "eframe"
    static member This = RegisterR "this"

and Instruction = 
    | Alloca of LLVMType
    | BitCast of LLVMType * Result * LLVMType
    | Br of Label
    | BrIf of Result * Label * Label
    | Call of string * (LLVMType*Result) list
    | CallPrim of Ast.Primitive *  (LLVMType*Result) list
    | CallClo of Result * int * (LLVMType * Result) // Func, packed args length, packed args
    | CallMethod of Result * Result * int * (LLVMType * Result) // Ctor, instance, packed args length, packed args
    | Comment of string
    | GetElementPtr of LLVMType * Result * int 
    | GetElementPtr2 of LLVMType * Result * int * int 
    | Icmp of Comp * Result * BoolTypes
    | Load of LLVMType * Result
    | Malloc of LLVMType
    | MarkLabel of Label
    | Or of LLVMType * Result * Result
    | PtrToInt of LLVMType * Result * LLVMType
    | Ret of Result
    | Store of LLVMType * Result * LLVMType * Result
and Label = Label of string
and Comp = 
    | Equal
    override this.ToString() =
        match this with
        | Equal -> "eq"
and LLVMType =
    | Int of int
    | Double
    | Pointer of LLVMType
    | Array of int * LLVMType
    | Struct of LLVMType list
    | Named of string * int
    override this.ToString() =
        match this with 
        | Int i -> sprintf "i%i" i
        | Double -> "double"
        | Pointer ty -> ty.ToString() + "*"
        | Array(cnt,ty) -> sprintf "[%i x %O]" cnt ty
        | Struct tys -> sprintf "{%s}" (String.concat ", " (Seq.map string tys))
        | Named(tyName,_) -> "%" + tyName
    member this.Size = 
        match this with 
        | Pointer _
        | Int _    -> 4
        | Double _ -> 8
        | Array(cnt,ty) -> cnt * ty.Size
        | Struct tys -> tys |> List.sumBy (fun ty -> ty.Size) 
        | Named(_,size) -> size
    member this.Ptr = Pointer this
    static member I32 = Int 32
    static member I8  = Int 8
    static member array len ty = Array(len,ty)
    static member zeroArrayI32 = LLVMType.array 0 LLVMType.I32
    static member stringArray len = Array(len+1,LLVMType.I8)
    static member stringPtr len = (LLVMType.stringArray len).Ptr
    static member EFrame = Named("EFrame", 8)
    static member SizedFrame size = 
        Struct [
            LLVMType.EFrame.Ptr
            LLVMType.array size LLVMType.I32
        ]
    static member Plain = LLVMType.Object []
    static member Object otherValues = 
        [
            LLVMType.I32; // Type
            LLVMType.I32.Ptr; //Fields
        ] @ otherValues
        |> Struct
    static member string len =
        LLVMType.Object [
            LLVMType.I32; //length
            LLVMType.array (len+1) LLVMType.I8 |> LLVMType.ptr // string *
        ]
    static member closureStruct = 
        LLVMType.Object [
            LLVMType.I32 // Closure ID
            LLVMType.EFrame.Ptr // Frame
        ]
    static member closure = Named("Closure", LLVMType.closureStruct.Size)
    static member ptr ty = ty.Ptr
    static member String0 = LLVMType.string -1

let i32 = LLVMType.I32
let i8 = LLVMType.I8
let String0 = LLVMType.String0

let GetFieldPtr (ty,from, f:FieldPostions) =
    GetElementPtr(ty,from,int(f))

let GetFieldPtrR (ty,from, f:FieldPostions) =
    GetElementPtrR(ty,from,int(f))

let GetField(obj,id)       = Call("GetField",[i32, obj; i32, id])
let SetField(obj,id,value) = Call("SetField",[i32, obj; i32, id; i32, value])
    
/// An active pattern to match certain types of results
let (|Register|NoResult|) a = 
    match a with
    | RegisterR _ -> Register a
    | NoResult -> NoResult 
    | _ -> failwith "Invalid result register"

/// A Partial active pattern to match certian types of Results
let (|Reg|_|) r =
    match r with 
    | RegisterR s -> Some s
    | _ -> None