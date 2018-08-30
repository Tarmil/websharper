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

module internal WebSharper.Compiler.CSharp.Scoping

open WebSharper.Core
open WebSharper.Core.AST

open System.Collections.Generic

type HasCapturedDeclarations() =
    inherit Visitor()

    let vars = HashSet()
    let mutable scope = 0
    let mutable hasCapture = false

    override this.VisitVarDeclaration (a, b) =
        if scope = 0 then
            vars.Add(a) |> ignore
        this.VisitExpression(b)

    override this.VisitFunction (_, b) =
        scope <- scope + 1    
        this.VisitStatement(b)
        scope <- scope - 1   

    override this.VisitId a =
        if scope > 0 && vars.Contains a then
            hasCapture <- true

    member this.Check a =
        this.VisitStatement a
        hasCapture

// 0: return
// 1: break
// 2: continue

let i0 = Value (Int 0)
let i1 = Value (Int 1)
let i2 = Value (Int 2)
    
type BlockScoping() =
    inherit Transformer()
    
    let mutable hasReturn = false
    let breaks = ResizeArray() 
    let conts = ResizeArray() 

    override this.TransformFunction(a, b) = VNone

    override this.TransformReturn(a) =
        match a with
        | Undefined -> Return(NewArray [ i0 ])
        | _ -> Return(NewArray [ i0; a ])
        |> VSome

    override this.TransformBreak(a) =
        breaks.Add a
        match a with 
        | None -> Return(NewArray [ i1 ]) 
        | Some l ->
            Return(NewArray [ i1; Value (String l.Name.Value) ])
        |> VSome

    override this.TransformContinue(a) =
        conts.Add a
        match a with 
        | None -> Return(NewArray [ i2 ]) 
        | Some l -> Return(NewArray [ i2; Value (String l.Name.Value) ])
        |> VSome

     member this.CloseBlock(a) =
        let res = Id.New()
        if hasReturn || breaks.Count > 0 || conts.Count > 0 then
            Block [
                VarDeclaration(res, Application(Function([], Block a), [], NonPure, Some 0))

                If (Var res, 
                    Switch(ItemGet(Var res, i0, Pure),
                        [
                            if hasReturn then 
                                yield (Some i0, Return (ItemGet(Var res, i1, Pure)))
                            if breaks.Count > 0 then 
                                yield (Some i1,
                                    Switch(ItemGet(Var res, i1, Pure),
                                        [
                                            for b in breaks do
                                                match b with
                                                | None ->
                                                    yield (Some Undefined, Break None)
                                                | Some l ->
                                                    yield (Some (Value (String l.Name.Value)), Break b)
                                        ]
                                    )
                                )
                            if conts.Count > 0 then 
                                yield (Some i2,
                                    Switch(ItemGet(Var res, i1, Pure),
                                        [
                                            for b in conts do
                                                match b with
                                                | None ->
                                                    yield (Some Undefined, Continue None)
                                                | Some l ->
                                                    yield (Some (Value (String l.Name.Value)), Continue b)
                                        ]
                                    )
                                )
                        ]
                    )
                , Empty)
            ]
        else ExprStatement <| Application(Function([], Block a), [], NonPure, Some 0)

type FixScoping() =
    inherit Transformer()
    
    member this.TransformLoopBody(a) =
        match IgnoreStatementSourcePos a with
        | Block bs -> 
            if HasCapturedDeclarations().Check(a) then
                BlockScoping().CloseBlock(bs) |> VSome
                // TODO : restore source pos info
            else VNone
        | _ -> VNone

    override this.TransformFor(a, b, c, d) =
        // TODO: there was no TransformExpression on a???
        match this.TransformExpressionOption a, this.TransformExpressionOption b, this.TransformExpressionOption c, VOption.chain [this.TransformStatement; this.TransformLoopBody] d with
        | VNone, VNone, VNone, VNone -> VNone
        | trA, trB, trC, trD -> For (trA.Or a, trB.Or b, trC.Or c, trD.Or d) |> VSome

    override this.TransformWhile(a, b) =
        match this.TransformExpression a, VOption.chain [this.TransformStatement; this.TransformLoopBody] b with
        | VNone, VNone -> VNone
        | trA, trB -> While (trA.Or a, trB.Or b) |> VSome

    override this.TransformDoWhile(a, b) =
        match VOption.chain [this.TransformStatement; this.TransformLoopBody] a, this.TransformExpression b with
        | VNone, VNone -> VNone
        | trA, trB -> DoWhile (trA.Or a, trB.Or b) |> VSome
    
let private fixInst = FixScoping() 

let fix s =  fixInst.TransformStatement s