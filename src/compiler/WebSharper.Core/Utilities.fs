// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2016 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}

namespace WebSharper.Core
  
open System
open System.Collections.Generic
open System.Globalization 
                  
/// A wrapper type for storing a computed hash for faster dictionary lookups
[<CustomEquality; CustomComparison; Struct>]
[<System.Diagnostics.DebuggerDisplay("{Value}")>]
type Hashed<'T when 'T : equality and 'T : comparison> =
    val Value : 'T
    val Hash : int 

    new v = { Value = v; Hash = hash v }

    override this.GetHashCode() =
        this.Hash

    override this.ToString() = sprintf "%A" this.Value
    
    override this.Equals(other: obj) : bool =
        match other with
        | :? Hashed<'T> as o ->
            this.Hash = o.Hash && (
                let v1 = this.Value
                let v2 = o.Value
                obj.ReferenceEquals(v1, v2) || v1 = v2
            )
        | _ -> failwith "invalid equality check"

    interface System.IComparable with
        member this.CompareTo (other: obj) =
            match other with
            | :? Hashed<'T> as o ->
                compare this.Value o.Value
            | _ -> failwith "invalid comparison"        

    static member Get (h: Hashed<'T>) = h.Value

/// Utility extensions of IDictionary and parsing value types
[<AutoOpen>]
module FSharpExtensions =
    type System.Collections.Generic.IDictionary<'K,'V> with
        member this.TryFind(key) =
            let mutable value = Unchecked.defaultof<'V>
            if this.TryGetValue(key, &value) then Some value else None

    type Int32 with
        static member FromString text =
            let ic = CultureInfo.InvariantCulture
            match Int32.TryParse(text, NumberStyles.Any, ic) with
            | true, x -> Some x
            | _ -> None

    type Int64 with
        static member FromString text =
            let ic = CultureInfo.InvariantCulture
            match Int64.TryParse(text, NumberStyles.Any, ic) with
            | true, x -> Some x
            | _ -> None

    type Double with
        static member FromString text =
            let ic = CultureInfo.InvariantCulture
            match Double.TryParse(text, NumberStyles.Any, ic) with
            | true, x -> Some x
            | _ -> None

/// Utility functions on IDictionary objects
module Dict =
    /// Returns true if the dictionary has no elements
    let isEmpty (d: IDictionary<_,_>) =
        d.Count = 0

    /// Add or append item to an IDictionary having a lists as values
    let addToMulti (d: IDictionary<_,_>) k v =
        match d.TryGetValue k with
        | true, p -> d.[k] <- v :: p
        | _ -> d.Add(k, [v])    
    
    /// Get the list or an empty list from an IDictionary having a lists as values
    let getFromMulti (d: IDictionary<_,_>) k =
        match d.TryGetValue k with
        | true, vs -> vs
        | _ -> []
    
    /// Filter an IDictionary
    let filter predicate (d: IDictionary<_,_>) =
        let r = Dictionary() :> IDictionary<_,_>
        for KeyValue(k, v) in d do   
            if predicate k v then r.Add(k, v)
        r 
    
    /// Map values of an IDictionary
    let map mapping (d: IDictionary<_,_>) =
        let r = Dictionary() :> IDictionary<_,_>
        for KeyValue(k, v) in d do   
            r.Add(k, mapping v)
        r 

    exception UnionError of key: obj with
        override this.Message = failwithf "Error merging dictionaries on key: %A" this.key 

    // Union of multiple IDictionary objects
    let union (dicts: seq<IDictionary<_,_>>) =
        let d = Dictionary() :> IDictionary<_,_>
        for s in dicts do
            for i in s do 
                try d.Add(i)
                with _ -> raise (UnionError i.Key)
        d
        
    // Union of multiple IDictionary objects, appending lists on key merges
    let unionAppend (dicts: seq<IDictionary<_,_>>) =
        let d = Dictionary() :> IDictionary<_,_>
        for s in dicts do
            for KeyValue(k, v) in s do 
                match d.TryFind k with
                | Some l ->
                    d.[k] <- List.append v l
                | _ ->
                    d.Add(k, v)
        d

    exception UnionDuplError of key: obj with
        override this.Message = failwithf "Different values found for the same key: %A" this.key 

    /// IDictionary union, allowing exact duplicates
    let unionDupl (dicts: seq<IDictionary<_,_>>) =
        let d = Dictionary() :> IDictionary<_,_>
        for s in dicts do    
            for KeyValue(k, v) as i in s do 
                match d.TryGetValue k with
                | true, ov ->
                    if v = ov then () else
                        raise (UnionDuplError k)
                | _ -> d.Add(i)
        d
  
    /// Swap the key and values of an IDictionary
    let swap (d: IDictionary<_,_>) =
        let r = Dictionary() :> IDictionary<_,_>
        for KeyValue(k, v) in d do   
            r.Add(v, k)
        r
    
    /// IDictionary.TryGetValue result converted to option
    let tryFind key (d: IDictionary<_,_>) =
        let mutable value = Unchecked.defaultof<'V>
        if d.TryGetValue(key, &value) then Some value else None

[<Struct>]
type VOption<'T> =
    | VNone
    | VSome of 'T

module VOption =

    let isSome = function
        | VNone -> false
        | VSome _ -> true

    let isNone = function
        | VNone -> true
        | VSome _ -> false

    let ofOption = function
        | None -> VNone
        | Some x -> VSome x

    let toOption = function
        | VNone -> None
        | VSome x -> Some x

    let map f = function
        | VNone -> VNone
        | VSome x -> VSome (f x)

    let defaultValue (def: 'T) (o: VOption<'T>) =
        match o with
        | VNone -> def
        | VSome x -> x

    /// Transform a value, or return it if the transformation returned VNone.
    let attempt (f: 'T -> VOption<'T>) (x: 'T) : 'T =
        match f x with
        | VNone -> x
        | VSome y -> y

    /// Returns VNone if f returns VNone for all items in l.
    /// Otherwise, returns VSome (l |> List.map (fun x -> defaultValue x (f x)))
    let bindList (f: 'T -> VOption<'T>) (l: list<'T>) : VOption<list<'T>> =
        let res, hasSome =
            List.mapFold (fun hasSome x ->
                let trX = f x
                defaultValue x trX, (hasSome || isSome trX)
            ) false l
        if hasSome then VSome res else VNone

    /// Optimized equivalent to `List.map (fun x -> defaultValue x (f x))`
    let bindList' (f: 'T -> VOption<'T>) (l: list<'T>) : list<'T> =
        defaultValue l (bindList f l)

    let bindOption (f: 'T -> VOption<'T>) (o: option<'T>) : VOption<option<'T>> =
        match o with
        | None -> VNone
        | Some x -> map Some (f x)

    let bindPair (f: 'T -> VOption<'T>) (g: 'U -> VOption<'U>) (x: 'T, y: 'U) : VOption<'T * 'U> =
        match f x, g y with
        | VNone, VNone -> VNone
        | vx, vy -> VSome (defaultValue x vx, defaultValue y vy)

    let chain (fs: list<'T -> VOption<'T>>) (init: 'T) : VOption<'T> =
        let res, hasSome =
            List.fold (fun (x, hasSome) f ->
                match f x with
                | VNone -> (x, hasSome)
                | VSome y -> (y, true)
            ) (init, false) fs
        if hasSome then VSome res else VNone

    let chain' (fs: list<'T -> VOption<'T>>) (init: 'T) : 'T =
        List.fold (fun x f -> attempt f x) init fs

type VOption<'T> with
    member v.Or(x: 'T) = VOption.defaultValue x v
