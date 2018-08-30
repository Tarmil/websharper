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

module WebSharper.Compiler.Optimizations

open System.Collections.Generic

open WebSharper.Core
open WebSharper.Core.AST
open WebSharper.Compiler

module I = WebSharper.Core.JavaScript.Identifier

open IgnoreSourcePos

//let (|GlobalAccess|_|) e =
//    let rec ga acc e =
//        match e with
//        | GlobalAccess a -> Some (Address (acc @ a.Value))
//        | ItemGet(m, Value (String f), _) -> ga (f :: acc) m
//        | _ -> None
//    match e with 
//    | GlobalAccess a -> Some a
//    | _ -> ga [] e

let (|Runtime|_|) e = 
    match e with 
    | GlobalAccess a ->
        match a.Value with
        | [ r; "Runtime"; "IntelliFactory" ] -> Some r
        | _ -> None
    | _ -> None

let (|Global|_|) e = 
    match e with 
    | GlobalAccess a ->
        match a.Value with
        | [ r ] -> Some r
        | _ -> None
    | _ -> None
    
let (|AppItem|_|) e =
    match e with
    | Application (ItemGet (obj, Value (String item), _), args, _, _) -> Some (obj, item, args)
    | _ -> None

let (|AppRuntime|_|) e =
    match e with
    | Application (Runtime rtFunc, args, _, _) -> Some (rtFunc, args)
    | _ -> None

let (|GetPrototypeConstuctor|_|) e =
    match e with
    | ItemGet(ItemGet(GlobalAccess m, Value (String "prototype"), _), Value (String "constructor"), _) -> Some m
    | _ -> None

let AppItem (obj, item, args) =
    Application(ItemGet(obj, Value (String item), Pure), args, NonPure, None)

let func vars body isReturn =
    if isReturn then Lambda(vars, body) else Function(vars, ExprStatement body)

let thisFunc this vars body isReturn =
    func vars (FixThisScope().Fix'(SubstituteVar(this, This).TransformExpression'(body))) isReturn

let globalArray = Address [ "Array" ]

let cleanRuntime force expr =
//    let tr = Transform clean
    match expr with
    | Application (Global "id", [ x ], _, _) -> 
        VSome x
    | Application (Global "ignore", [ x ], _, _) -> 
        Unary(UnaryOperator.``void``, x) |> VSome
    | Application (AppRuntime ("Bind", [f; obj]), args, _, _) -> 
        AppItem(f, "call", obj :: args) |> VSome
    | Application(Application(AppRuntime("Curried2", [ f ]), [ a ], _, _), [ b ], isPure, _) ->
        Application(f, [ a; b ], isPure, Some 2) |> VSome
    | Application(Application(Application(AppRuntime("Curried3", [ f ]), [ a ], _, _), [ b ], _, _), [ c ], isPure, _) ->
        Application(f, [ a; b; c ], isPure, Some 3) |> VSome

    | AppItem(NewArray arr, "concat", [ NewArray rest ]) ->
        NewArray (arr @ rest) |> VSome
    | AppRuntime(rtFunc, xs) ->
        match rtFunc, xs with
        | "Apply", [AppRuntime ("Bind", [f; obj]); ignoredObj; args] ->
            AppItem(f, "apply", [obj; Sequential [ignoredObj; args]]) |> VSome
        
        //used by functions with rest argument
        | "Apply", [GlobalAccess mf; Value Null ] ->
            Application (GlobalAccess mf, [], NonPure, None) |> VSome
        | "Apply", [GlobalAccess mf; Value Null; NewArray arr ] ->
            Application (GlobalAccess mf, arr, NonPure, None) |> VSome
        | "Apply", [GlobalAccess mf; Value Null; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] ->
            Application (GlobalAccess mf, arr @ rest, NonPure, None) |> VSome

        | "Apply", [GlobalAccess mf; GlobalAccess m ] when mf.Value.Tail = m.Value ->
            Application (GlobalAccess mf, [], NonPure, None) |> VSome
        | "Apply", [GlobalAccess mf; GlobalAccess m; NewArray arr ] when mf.Value.Tail = m.Value ->
            Application (GlobalAccess mf, arr, NonPure, None) |> VSome
        | "Apply", [GlobalAccess mf; GlobalAccess m; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] when mf.Value = m.Value ->
            Application (GlobalAccess mf, arr @ rest, NonPure, None) |> VSome
        
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2 ] when m1 = m2 ->
            if m1 = globalArray then NewArray []
            else New(GlobalAccess m1, [])
            |> VSome
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2; NewArray arr ] when m1 = m2 ->
            if m1 = globalArray then NewArray arr
            else New(GlobalAccess m1, arr)
            |> VSome
        | "Apply", [GetPrototypeConstuctor m1; GlobalAccess m2; AppItem(NewArray arr, "concat", [ NewArray rest ]) ] when m1 = m2 ->
            if m1 = globalArray then NewArray (arr @ rest)
            else New(GlobalAccess m1, arr @ rest)
            |> VSome

        | "Apply", [ Application(Runtime "Curried", [f; Value (Int l)], isPure, _); ignoredObj; NewArray args ] 
            when args.Length = l && isPureExpr ignoredObj ->
                Application(f, args, isPure, Some l) |> VSome
        | "Apply", [f; obj; args] when force ->
            AppItem(f, "apply", [ obj; args ]) |> VSome
        | "Apply", [f; obj] when force ->
            AppItem(f, "apply", [ obj ]) |> VSome
        | "CreateFuncWithArgs", [ TupledLambda (vars, body, isReturn) as f ] ->
            func vars body isReturn |> WithSourcePosOfExpr f |> VSome
        | "CreateFuncWithArgs", _ ->
#if DEBUG
            printfn "non-optimized CreateFuncWithArgs: %A" (Debug.PrintExpression expr)
#endif
            VNone
        | "CreateFuncWithOnlyThis", [ Lambda ([obj], body, isReturn) as f ] ->
            thisFunc obj [] body isReturn |> WithSourcePosOfExpr f |> VSome
        | "CreateFuncWithThis", [ Lambda ([obj], Lambda (args, body, isReturn), true) as f ] ->
            thisFunc obj args body isReturn |> WithSourcePosOfExpr f |> VSome
        | "CreateFuncWithThis", [ AppRuntime ("Curried", [Lambda([obj; arg], body, isReturn) as f]) ] ->
            thisFunc obj [arg] body isReturn |> WithSourcePosOfExpr f |> VSome
        | "CreateFuncWithThisArgs", [ Lambda ([obj], TupledLambda (vars, body, isReturn), true) as f ] ->
            thisFunc obj vars body isReturn |> WithSourcePosOfExpr f |> VSome
        | "CreateFuncWithThisArgs", [ AppRuntime ("Curried", [Lambda([obj; arg], body, isReturn) as f]) ] ->
            match func [arg] body isReturn with
            | TupledLambda(vars, body, _) ->
                thisFunc obj vars body isReturn |> WithSourcePosOfExpr f
            | _ ->
                thisFunc obj [arg] body isReturn |> WithSourcePosOfExpr f
            |> VSome
        | "CreateFuncWithRest", [ Value (Int length); TupledLambda (vars, body, isReturn) as f ] ->
            match List.rev vars with
            | rest :: fixRev ->
                let fix = List.rev fixRev
                if containsVar rest body then
                    func fix (Let (rest, sliceFromArguments [ length ], body)) isReturn |> WithSourcePosOfExpr f
                else
                    func fix body isReturn |> WithSourcePosOfExpr f
                |> VSome
            | _ -> VNone
        | "SetOptional", [obj; field; optValue] ->
            match optValue with
            | Object ["$", Value (Int 0)] ->
                MutatingUnary(MutatingUnaryOperator.delete, ItemGet(obj, field, NonPure)) |> WithSourcePosOfExpr expr |> VSome
            | Object ["$", Value (Int 1); "$0", value] ->
                ItemSet (obj, field, value) |> WithSourcePosOfExpr expr |> VSome
            | _ -> VNone
        | "SetOrDelete", [obj; field; value] ->
            if isTrivialValue value then
                match value with
                | Undefined ->
                    MutatingUnary(MutatingUnaryOperator.delete, ItemGet(obj, field, NonPure)) |> WithSourcePosOfExpr expr |> VSome
                | _ ->
                    ItemSet (obj, field, value) |> WithSourcePosOfExpr expr |> VSome
            else VNone
        | "NewObject", [NewArray keyValuePairs] ->
            let withConstantKey =
                keyValuePairs |> List.choose (function 
                    | NewArray [Value (String k); v] -> Some (k, v) 
                    | _ -> None)
            if withConstantKey.Length = keyValuePairs.Length then
                Object (withConstantKey |> List.map (fun (k, v) -> k, v)) |> WithSourcePosOfExpr expr |> VSome
            else VNone
        | "DeleteEmptyFields", [Object fs; NewArray names] ->
            let toDelete = HashSet (names |> Seq.choose (function Value (String n) -> Some n | _ -> None))
            if names.Length = toDelete.Count then
                let remaining = ResizeArray()
                let rec alwaysHasValue e =
                    match e with
                    | Arguments        
                    | Value _
                    | Function _            
                    | New _               
                    | NewArray _          
                    | Object _ -> true        
                    | Let (_, _, b)       
                    | LetRec (_, b) -> alwaysHasValue b     
                    | Sequential b -> alwaysHasValue (List.last b)     
                    | _ -> false    
                for (n, v) in fs do
                    if toDelete.Contains n then
                        if v = Undefined then 
                            toDelete.Remove n |> ignore
                        else
                            if alwaysHasValue v then
                                toDelete.Remove n |> ignore
                            remaining.Add (n, v)
                    else remaining.Add (n, v)
                let obj = Object (List.ofSeq remaining)
                if toDelete.Count = 0 then
                    obj
                else                  
                    JSRuntime.DeleteEmptyFields obj [for f in toDelete -> !~(String f)]
                |> VSome
            else VNone
        | _ -> VNone
    | Let (var, value, body) ->
        //transform function if it is always used as JavaScript interop
        let transformIfAlwaysInterop rtFunc getJsFunc =
            let (|WithInterop|_|) e =
                match e with
                | Application (Runtime f, [ Var v ], _, _) when f = rtFunc && v = var -> Some ()
                | _ -> None
            let rec isWithInterop e =
                match e with
                | WithInterop -> Some true
                | Var v when v = var -> Some false
                | _ -> None
            if ForAllSubExpr(isWithInterop).Check(body) then
                Let(var, getJsFunc() |> WithSourcePosOfExpr value, 
                    body |> BottomUp (function WithInterop -> Var var | e -> e))
                |> VSome
            else VNone
        match value with
        | TupledLambda (vars, lBody, isReturn) ->
            transformIfAlwaysInterop "CreateFuncWithArgs" (fun () -> func vars lBody isReturn)
        | Lambda ([obj], Lambda (args, lBody, isReturn), true) ->
            transformIfAlwaysInterop "CreateFuncWithThis" (fun () -> thisFunc obj args lBody isReturn)
        | Lambda ([obj], lBody, isReturn) ->
            transformIfAlwaysInterop "CreateFuncWithOnlyThis" (fun () -> thisFunc obj [] lBody isReturn)
        | Lambda ([obj], TupledLambda (vars, lBody, isReturn), true) ->
            transformIfAlwaysInterop "CreateFuncWithThisArgs" (fun () -> thisFunc obj vars lBody isReturn)
        | _ ->
            VNone
    | ItemGet (Object fs, Value (String fieldName), _) when not (I.IsObjectMember fieldName) ->
        let mutable nonPureBefore = []
        let mutable nonPureAfter = []
        let mutable fieldValue = None
        for n, v in fs do
            if n = fieldName then
                fieldValue <- Some v
            else 
                if not (isPureExpr v) then
                    match fieldValue with
                    | None -> nonPureBefore <- v :: nonPureBefore
                    | _ -> nonPureAfter <- v :: nonPureAfter
        let fieldValue = defaultArg fieldValue Undefined
        let result =
            Sequential (List.rev (fieldValue :: nonPureBefore))
        if List.isEmpty nonPureAfter then
            result 
        else 
            let resVar = Id.New (fieldName, false)
            Let (resVar, result, 
                Sequential (List.rev (Var resVar :: nonPureAfter))
            )
        |> VSome
    | ItemGet (NewArray fs, Value (Int index), _) ->
        let mutable nonPureBefore = []
        let mutable nonPureAfter = []
        let mutable fieldValue = None
        let mutable i = 0
        for v in fs do
            if i = int index then
                fieldValue <- Some v
            else 
                if not (isPureExpr v) then
                    match fieldValue with
                    | None -> nonPureBefore <- v :: nonPureBefore
                    | _ -> nonPureAfter <- v :: nonPureAfter
            i <- i + 1
        let fieldValue = defaultArg fieldValue Undefined
        let result =
            Sequential (List.rev (fieldValue :: nonPureBefore))
        if List.isEmpty nonPureAfter then
            result 
        else 
            let resVar = Id.New ("item" + string index, false)
            Let (resVar, result, 
                Sequential (List.rev (Var resVar :: nonPureAfter))
            )
        |> VSome
    // created by FSharpRef if using record constructor
    | Object [ "0", x ] ->
        NewArray [ x ] |> VSome
    | _ -> VNone

let cleanRuntime' force expr =
    VOption.attempt (cleanRuntime force) expr
