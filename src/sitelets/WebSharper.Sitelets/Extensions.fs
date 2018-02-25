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

/// Implements utilities for use by the current assembly.
namespace WebSharper.Sitelets

open System.Diagnostics
open System.Threading.Tasks
open WebSharper

[<AutoOpen>]
module internal Extensions =

    let private source =
        TraceSource("WebSharper", SourceLevels.All)

    let Timed message action =
        let sw = Stopwatch()
        sw.Start()
        let r = action()
        source.TraceInformation("{0} in {1} sec.",
            message, sw.Elapsed.TotalSeconds)
        r

    let startsWithSlash (s: string) =
        s.Length > 0
        && s.[0] = '/'

    let endsWithSlash (s: string) =
        s.Length > 0
        && s.[s.Length - 1] = '/'

    let joinWithSlash (a: string) (b: string) =
        match endsWithSlash a, startsWithSlash b with
        | true, true -> a + b.Substring(1)
        | false, false -> a + "/" + b
        | _ -> a + b

    let appendSlash (s: string) =
        if endsWithSlash s then s else s + "/"

    [<Inline>]
    let internal ofObjNoConstraint (x: 'T) =
        if obj.ReferenceEquals(x, null) then None else Some x

    type Task<'A> with

        member this.CopyInto(tcs: TaskCompletionSource<'A>) =
            if this.IsFaulted then tcs.TrySetException(this.Exception.InnerExceptions) |> ignore
            elif this.IsCanceled then tcs.TrySetCanceled() |> ignore
            else tcs.TrySetResult(this.Result) |> ignore

        member t.Map (f: 'A -> 'B) =
            t.ContinueWith(
                (fun (t: Task<'A>) -> f t.Result),
                TaskContinuationOptions.OnlyOnRanToCompletion
            )

        // https://blogs.msdn.microsoft.com/pfxteam/2010/11/21/processing-sequences-of-asynchronous-operations-with-tasks/
        member this.Bind(next: 'T -> Task<'U>) : Task<'U> =
            let tcs = new TaskCompletionSource<'U>()
            this.ContinueWith(
                (fun (t: Task<'T>) ->
                    if t.IsFaulted then tcs.TrySetException(t.Exception.InnerExceptions) |> ignore
                    elif t.IsCanceled then tcs.TrySetCanceled() |> ignore
                    else
                        try
                            match next(t.Result) with
                            | null -> tcs.TrySetCanceled() |> ignore
                            | n -> n.ContinueWith(fun (n: Task<'U>) -> n.CopyInto(tcs)) |> ignore
                        with e ->
                            tcs.TrySetException(e) |> ignore
                ),
                TaskContinuationOptions.ExecuteSynchronously
            ) |> ignore
            tcs.Task
