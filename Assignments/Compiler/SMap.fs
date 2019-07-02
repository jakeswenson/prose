namespace Utils

/// Represents a Scoped Map
/// That is: A map of keys to values that also has a parent field that represnts the parent Scope
type SMap<'key,'value when 'key : comparison> =
    { Map : Map<'key,'value>
      Parent : SMap<'key,'value> option;
      mutable ClosureCount: int }
    member this.Add key value =
        { this with Map = Map.add key value this.Map }
    member this.Contains key = 
        if not <| this.Map.ContainsKey key then
            match this.Parent with
            | Some parent -> parent.Contains key
            | None -> false 
        else true
    member this.UpdateContainingScope var k=
        if this.Map.ContainsKey var then
            k this
        else 
            if this.Parent.IsSome 
            then this.Parent.Value.UpdateContainingScope var k
    member this.InLocalScope key = this.Map.ContainsKey key
    member this.Push() = { Map = Map.empty; ClosureCount = 0;  Parent = Some this; }
    member this.Lookup k =
        match this.Map.TryFind k,this.Parent with
        | Some v, _ -> (0,v)
        | None, Some p ->
            match p.Lookup k with
            | (frame, value) -> ((if this.ClosureCount > 0 then frame + 1 else frame), value)
        | None, None -> failwithf "A variable named %A was not found in this scope" k
    member this.Finish() =
        match this.Parent with
        | Some map -> map
        | None -> failwith "Not in a nested scope!"
    member this.Item
        with get key = 
            match this.Map.TryFind key with
            | Some value -> value
            | None -> 
                if this.Parent.IsSome then
                    this.Parent.Value.[key]
                else failwithf "Key not found: %A in %A " key this

/// Scoped Map Utilities
module SMap = 
    let empty<'a,'b when 'a:comparison> = { Map = Map.empty<'a,'b>; Parent = None; ClosureCount = 0; }
    let add key value (map:SMap<_,_>) = map.Add key value
