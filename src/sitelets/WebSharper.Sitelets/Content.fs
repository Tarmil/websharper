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

namespace WebSharper.Sitelets

open System.IO
open System.Threading.Tasks
open WebSharper
module CT = WebSharper.Core.ContentTypes

[<Struct; CompiledName "FSharpContent">]
type Content<'T> =
    | ConstantContent of constantResponse: Http.Response
    | CustomContent of response: (Context<'T> -> Http.Response)
    | CustomContentAsync of responseAsync: (Context<'T> -> Task<Http.Response>)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Content =
    open System
    open System.Collections.Generic
    open System.IO
    open System.Text.RegularExpressions
    type private HtmlTextWriter = WebSharper.Core.Resources.HtmlTextWriter

    type private Func<'A,'B> = System.Func<'A,'B>

    module M = WebSharper.Core.Metadata
    module J = WebSharper.Core.Json

    let metaJson<'T> (m: M.Info) (jP: Core.Json.Provider) (controls: seq<IRequiresResources>) =
        controls
        |> List.ofSeq
        |> List.collect (fun c -> c.Encode(m, jP))
        |> J.Encoded.Object
        |> jP.Pack
        |> J.Stringify

    let escape (s: string) =
        Regex.Replace(s, @"[&<>']",
            new MatchEvaluator(fun m ->
                match m.Groups.[0].Value.[0] with
                | '&'-> "&amp;"
                | '<'-> "&lt;"
                | '>' -> "&gt;"
                | '\'' -> "&#39;"
                | _ -> failwith "unreachable"))

    let writeResources (ctx: Web.Context) (controls: seq<#IRequiresResources>) (tw: Core.Resources.RenderLocation -> HtmlTextWriter) =
        // Resolve resources for the set of types and this assembly
        // Some controls may depend on Requires called first and Encode second, do not break this
        let resources =
            let nodeSet =
                controls
                |> Seq.collect (fun c -> c.Requires ctx.Metadata)
                |> Set
            ctx.ResourceContext.ResourceDependencyCache.GetOrAdd(nodeSet, fun nodes ->
                ctx.Dependencies.GetResources nodes
            )
        let hasResources = not (List.isEmpty resources)
        if hasResources then
            // Meta tag encoding the client side controls
            let mJson = metaJson ctx.Metadata ctx.Json (Seq.cast controls)
            // Render meta
            (tw Core.Resources.Meta).WriteLine(
                "<meta id='{0}' name='{0}' content='{1}' />",
                Activator.META_ID,
                escape mJson
            )
            // Render resources
            for r in resources do
                Core.Resources.Rendering.RenderCached(ctx.ResourceContext, r, tw)
        hasResources

    let writeStartScript (tw: HtmlTextWriter) =
        tw.WriteLine(@"<script type='{0}'>", CT.Text.JavaScript.Text)
        tw.WriteLine @"if (typeof IntelliFactory !=='undefined')"
        tw.WriteLine @"  IntelliFactory.Runtime.Start();"
        tw.WriteLine @"</script>"

    type RenderedResources =
        {
            Scripts : string
            Styles : string
            Meta : string
        }

        member this.Item
            with get (x: string) =
                match x.ToLowerInvariant() with
                | "scripts" -> this.Scripts
                | "styles" -> this.Styles
                | "meta" -> this.Meta
                | _ -> failwith "Invalid rendered resource identifier"

    let getSeparateResourcesAndScripts ctx controls : RenderedResources =
        use scriptsW = new StringWriter()
        let scriptsTw = new HtmlTextWriter(scriptsW, " ")
        use stylesW = new StringWriter()
        let stylesTw = new HtmlTextWriter(stylesW, " ")
        use metaW = new StringWriter()
        let metaTw = new HtmlTextWriter(metaW, " ")
        let hasResources =
            writeResources ctx controls (function
                | Core.Resources.Scripts -> scriptsTw
                | Core.Resources.Styles -> stylesTw
                | Core.Resources.Meta -> metaTw)
        if hasResources then
            writeStartScript scriptsTw
        {
            Scripts = scriptsW.ToString()
            Styles = stylesW.ToString()
            Meta = metaW.ToString()
        }

    let getResourcesAndScripts ctx controls =
        use w = new StringWriter()
        use tw = new HtmlTextWriter(w, " ")
        let hasResources = writeResources ctx controls (fun _ -> tw)
        if hasResources then writeStartScript tw
        w.ToString()

    let toCustomContent (htmlPage: Page) context : Http.Response =
        let writeBody (stream: Stream) =
            let body = Seq.cache htmlPage.Body
            let renderHead (tw: HtmlTextWriter) =
                let hasResources = writeResources context body (fun _ -> tw)
                for elem in htmlPage.Head do
                    elem.Write(context, tw)
                if hasResources then writeStartScript tw
            let renderBody (tw: HtmlTextWriter) =
                for elem in body do
                    elem.Write(context, tw)
            // Create html writer from stream
            use textWriter = new StreamWriter(stream)
            textWriter.AutoFlush <- true
            use htmlWriter = new HtmlTextWriter(textWriter, " ")
            htmlPage.Renderer htmlPage.Doctype htmlPage.Title
                renderHead renderBody htmlWriter
        {
            Status = Http.Status.Ok
            Headers = [Http.Header.Custom "Content-Type" "text/html; charset=utf-8"]
            WriteBody = writeBody
        }

    let JsonProvider = WebSharper.Core.Json.Provider.Create()

    let ToResponseTask<'T> (c: Content<'T>) (ctx: Context<'T>) : Task<Http.Response> =
        match c with
        | ConstantContent x -> Task.FromResult x
        | CustomContent x -> Task.FromResult (x ctx)
        | CustomContentAsync x -> x ctx

    let FromContext f =
        CustomContentAsync <| fun ctx ->
            ToResponseTask (f ctx) ctx

    let FromContextAsync (f: Context<'T> -> Task<Content<'T>>) =
        CustomContentAsync <| fun ctx ->
            f(ctx).Bind(fun c -> ToResponseTask c ctx)

    let ToResponse c ctx = ToResponseTask c ctx |> Async.AwaitTask

    let FromTask (ac: Task<Content<'T>>) : Content<'T> =
        CustomContentAsync <| fun ctx ->
            ac.Bind(fun t -> ToResponseTask t ctx)

    let FromAsync (ac: Async<Content<'T>>) : Content<'T> =
        CustomContentAsync <| fun ctx ->
            Async.StartAsTask(ac)
                .Bind(fun t -> ToResponseTask t ctx)

    let delay1 f =
        fun arg -> async { return f arg }

    let MapResponseTask<'T> (f: Http.Response -> Task<Http.Response>) (content: Content<'T>) =
        let genResp =
            match content with
            | ConstantContent x -> fun _ -> Task.FromResult(x)
            | CustomContent gen -> fun ctx -> Task.FromResult(gen ctx)
            | CustomContentAsync gen -> gen
        CustomContentAsync <| fun ctx ->
            genResp(ctx).Bind(f)

    let MapResponseAsync<'T> (f: Http.Response -> Async<Http.Response>) (content: Content<'T>) =
        MapResponseTask (f >> Async.StartAsTask) content

    let MapResponse<'T> (f: Http.Response -> Http.Response) (content: Content<'T>) =
        match content with
        | ConstantContent x -> ConstantContent (f x)
        | CustomContent gen -> CustomContent (gen >> f)
        | CustomContentAsync gen -> CustomContentAsync <| fun ctx -> gen(ctx).Map(f)

    let SetHeaders<'T> (headers: seq<Http.Header>) (cont: Content<'T>) =
        cont
        |> MapResponse (fun resp -> { resp with Headers = headers })

    let WithHeaders<'T> (headers: seq<Http.Header>) (cont: Content<'T>) =
        cont
        |> MapResponse (fun resp ->
            let headers = (List.ofSeq headers) @ (List.ofSeq resp.Headers)
            { resp with Headers = headers }
        )

    let WithHeader<'T> (name: string) (value: string) (cont: Content<'T>) =
        cont |> WithHeaders [Http.Header.Custom name value]

    let WithContentType<'T> (contentType: string) (cont: Content<'T>) =
        cont |> WithHeaders [Http.Header.Custom "Content-Type" contentType]

    let SetStatus<'T> (status: Http.Status) (cont: Content<'T>) =
        cont
        |> MapResponse (fun resp -> { resp with Status = status })

    let SetBody<'T> (writeBody: System.IO.Stream -> unit) (cont: Content<'T>) =
        cont
        |> MapResponse (fun resp -> { resp with WriteBody = writeBody })

    let private redirectToUrlResponse url =
        {
            Status = Http.Status.Custom 301 (Some "Moved Permanently")
            Headers = [Http.Header.Custom "Location" url]
            WriteBody = ignore
        } : Http.Response

    let private redirectTemporaryToUrlResponse url =
        {
            Status = Http.Status.Custom 307 (Some "Temporary Redirect")
            Headers = [Http.Header.Custom "Location" url]
            WriteBody = ignore
        } : Http.Response

    /// Emits a 301 Moved Permanently response to a given URL.
    let RedirectToUrl<'T> (url: string) : Content<'T> =
        ConstantContent <| redirectToUrlResponse url

    /// Emits a 301 Moved Permanently response to a given action.
    let Redirect<'T> (action: 'T) =
        CustomContent <| fun ctx -> redirectToUrlResponse (ctx.Link action)

    let RedirectPermanentToUrl url = RedirectToUrl url
    let RedirectPermanent url = Redirect url

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporaryToUrl<'T> (url: string) : Content<'T> =
        ConstantContent <| redirectTemporaryToUrlResponse url

    /// Emits a 307 Redirect Temporary response to a given url.
    let RedirectTemporary<'T> (action: 'T) : Content<'T> =
        CustomContent <| fun ctx -> redirectTemporaryToUrlResponse (ctx.Link action)

    /// Constructs a status code response.
    let httpStatusContent status : Content<'T> =
        ConstantContent {
            Status = status
            Headers = []
            WriteBody = ignore
        }

    let Unauthorized<'T> : Content<'T> =
        httpStatusContent Http.Status.Unauthorized

    let Forbidden<'T> : Content<'T> =
        httpStatusContent Http.Status.Forbidden

    let NotFound<'T> : Content<'T> =
        httpStatusContent Http.Status.NotFound

    let NotImplemented<'T> : Content<'T> =
        httpStatusContent Http.Status.NotImplemented

    let ServerError<'T> : Content<'T> =
        httpStatusContent Http.Status.InternalServerError

    let MethodNotAllowed<'T> : Content<'T> =
        httpStatusContent Http.Status.MethodNotAllowed

    let Ok<'T> : Content<'T> =
        httpStatusContent Http.Status.Ok

[<System.Runtime.CompilerServices.Extension; Sealed>]
type ContextExtensions =

    [<System.Runtime.CompilerServices.Extension>]
    static member GetSeparateResourcesAndScripts(ctx, controls) =
        Content.getSeparateResourcesAndScripts ctx controls

    [<System.Runtime.CompilerServices.Extension>]
    static member GetResourcesAndScripts(ctx, controls) =
        Content.getResourcesAndScripts ctx controls

type Content<'T> with

    static member Custom (response: Http.Response) : Content<'T> =
        ConstantContent response

    static member Custom (?Status: Http.Status, ?Headers: seq<Http.Header>, ?WriteBody: System.IO.Stream -> unit) : Content<'T> =
        ConstantContent {
            Status = defaultArg Status Http.Status.Ok
            Headers = defaultArg Headers Seq.empty
            WriteBody = defaultArg WriteBody ignore
        }

    static member Json (value: 'U) : Content<'T> =
        let encoder = Content.JsonProvider.GetEncoder<'U>()
        ConstantContent {
            Status = Http.Status.Ok
            Headers = [Http.Header.Custom "Content-Type" "application/json"]
            WriteBody = fun s ->
                use tw = new StreamWriter(s)
                value
                |> encoder.Encode
                |> Content.JsonProvider.Pack
                |> WebSharper.Core.Json.Write tw
        }

    static member Page (?Body: #seq<#WebSharper.Web.INode>, ?Head:#seq<#WebSharper.Web.INode>, ?Title: string, ?Doctype: string) : Content<'T> =
        Content.Page {
            Doctype = Some (match Doctype with Some d -> d | None -> "<!DOCTYPE html>")
            Title = Title
            Head = match Head with None -> Seq.empty | Some x -> Seq.cast x
            Body = match Body with None -> Seq.empty | Some x -> Seq.cast x
            Renderer = Page.Default.Renderer
        }

    static member Page (page: Page) : Content<'T> =
        CustomContent (Content.toCustomContent page)

    static member Text (text: string, ?encoding: System.Text.Encoding) : Content<'T> =
        let encoding = defaultArg encoding System.Text.Encoding.UTF8
        Content.Custom(
            WriteBody = fun s ->
                use w = new System.IO.StreamWriter(s, encoding)
                w.Write(text)
        )

    static member File (path: string, ?AllowOutsideRootFolder: bool, ?ContentType) : Content<'T> =
        CustomContent <| fun ctx ->
            let allowOutsideRootFolder = defaultArg AllowOutsideRootFolder false
            if Path.IsPathRooted path && not allowOutsideRootFolder then
                failwith "Cannot serve file from outside the application's root folder"
            let rootFolder = DirectoryInfo(ctx.RootFolder).FullName
            let path =
                if path.StartsWith "~/" || path.StartsWith @"~\" then
                    Path.Combine(rootFolder, path.[2..])
                else
                    Path.Combine(rootFolder, path)
            let fi = System.IO.FileInfo(path)
            if fi.FullName.StartsWith rootFolder || allowOutsideRootFolder then
                {
                    Status = Http.Status.Ok
                    Headers = [if ContentType.IsSome then yield Http.Header.Custom "Content-Type" ContentType.Value]
                    WriteBody = fun out ->
                        use inp = fi.OpenRead()
                        let buffer = Array.zeroCreate (16 * 1024)
                        let rec loop () =
                            let read = inp.Read(buffer, 0, buffer.Length)
                            if read > 0 then out.Write(buffer, 0, read); loop ()
                        loop ()
                }
            else
                failwith "Cannot serve file from outside the application's root folder"

type ContentBuilder =

    member this.Bind(c: Async<'U>, f: 'U -> Content<'T>) : Content<'T> =
        this.Bind(Async.StartAsTask(c), f)

    member this.Bind(c: Task<'U>, f: 'U -> Content<'T>) : Content<'T> =
        CustomContentAsync <| fun ctx ->
            c.Bind(fun c -> Content.ToResponseTask (f c) ctx)

    member this.Return(c: Content<'T>) : Content<'T> = c

    member this.ReturnFrom(c: Content<'T>) : Content<'T> = c

    member this.Delay(f: unit -> Content<'T>) : Content<'T> = f()

    member this.TryFinally(c: Content<'T>, f: unit -> unit) : Content<'T> =
        match c with
        | ConstantContent _ -> c
        | CustomContent g ->
            CustomContent <| fun ctx ->
                try g ctx finally f()
        | CustomContentAsync g ->
            CustomContentAsync <| fun ctx ->
                let tcs = TaskCompletionSource<Http.Response>()
                g(ctx).ContinueWith(fun (t: Task<Http.Response>) ->
                    f()
                    t.CopyInto(tcs)
                ) |> ignore
                tcs.Task

    member this.TryWith(c: Content<'T>, f: exn -> Content<'T>) : Content<'T> =
        match c with
        | ConstantContent _ -> c
        | CustomContent g ->
            Content.CustomContentAsync <| fun ctx ->
                try Task.FromResult(g ctx)
                with e -> Content.ToResponseTask (f e) ctx
        | CustomContentAsync g ->
            Content.CustomContentAsync <| fun ctx ->
                let tcs = TaskCompletionSource<Http.Response>()
                g(ctx).ContinueWith(
                    (fun (t: Task<Http.Response>) ->
                        if t.IsFaulted then
                            (Content.ToResponseTask (f t.Exception) ctx)
                                .ContinueWith(fun (t: Task<Http.Response>) ->
                                    t.CopyInto(tcs)
                                ) |> ignore
                        else
                            tcs.TrySetResult(t.Result) |> ignore
                    ),
                    TaskContinuationOptions.NotOnCanceled
                ) |> ignore
                tcs.Task

[<AutoOpen>]
module ContentBuilder =

    let content = Unchecked.defaultof<ContentBuilder>

open System
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

#nowarn "49" // allow uppercase parameter names

[<CompiledName "Content"; Struct; Extension; NoEquality; NoComparison>]
type CSharpContent =

    val private c: Content<obj>

    new(c) = { c = c }

    static member ToContent (c: Content<obj>) : Task<CSharpContent> =
        Task.FromResult(CSharpContent c)

    static member Opt x =
        match box x with
        | null -> None
        | _ -> Some x

    member this.AsContent = this.c

    static member TaskAsContent (t: Task<CSharpContent>) =
        t.Map(fun c -> c.AsContent)

    static member Json<'U> (x: 'U) : Task<CSharpContent> =
        Content.Json x
        |> CSharpContent.ToContent

    static member Page
        (
            [<Optional>] Body: Web.INode,
            [<Optional>] Head: Web.INode,
            [<Optional>] Title: string,
            [<Optional>] Doctype: string
        ) =
        Content.Page(
            ?Body = Option.map Seq.singleton (CSharpContent.Opt Body),
            ?Head = Option.map Seq.singleton (CSharpContent.Opt Head),
            ?Title = CSharpContent.Opt Title, ?Doctype = CSharpContent.Opt Doctype)
        |> CSharpContent.ToContent

    static member Page (page: Page) =
        Content.Page(page)
        |> CSharpContent.ToContent

    static member Text (text: string, [<Optional>] encoding: System.Text.Encoding) =
        Content.Text(text, ?encoding = CSharpContent.Opt encoding)
        |> CSharpContent.ToContent

    static member File (path: string, [<Optional>] AllowOutsideRootFolder: bool, [<Optional>] ContentType: string) =
        Content.File(path, AllowOutsideRootFolder, ?ContentType = CSharpContent.Opt ContentType)
        |> CSharpContent.ToContent

    static member Custom (response: Http.Response) =
        Content.Custom(response)
        |> CSharpContent.ToContent

    static member Custom
        (
            [<Optional>] Status: Http.Status,
            [<Optional>] Headers: seq<Http.Header>,
            [<Optional>] WriteBody: Action<Stream>
        ) =
        Content.Custom(?Status = CSharpContent.Opt Status, ?Headers = CSharpContent.Opt Headers, WriteBody = WriteBody.Invoke)
        |> CSharpContent.ToContent

    static member FromContext (f: Func<Context, Task<CSharpContent>>) =
        Content.FromContextAsync (fun x ->
            f.Invoke(Context x).Map(fun c -> c.AsContent))
        |> CSharpContent.ToContent

    [<Extension>]
    static member ToResponse (content: CSharpContent, context: Context) =
        Content.ToResponseTask content.AsContent context

    static member FromTask (content: Task<CSharpContent>) =
        CustomContentAsync <| fun ctx ->
            content.Bind(fun c -> Content.ToResponseTask c.AsContent ctx)
        |> CSharpContent

    [<Extension>]
    static member MapResponse (content: Task<CSharpContent>, f: Func<Http.Response, Http.Response>) =
        content.Map(fun c -> CSharpContent(Content.MapResponse f.Invoke c.AsContent))

    [<Extension>]
    static member MapResponseAsync (content: Task<CSharpContent>, f: Func<Http.Response, Task<Http.Response>>) =
        content.Map(fun c -> CSharpContent(Content.MapResponseTask f.Invoke c.AsContent))

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content.Map(fun c -> CSharpContent(Content.WithHeaders headers c.AsContent))

    [<Extension>]
    static member WithHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content.Map(fun c -> CSharpContent(Content.WithHeaders headers c.AsContent))

    [<Extension>]
    static member WithHeader (content: Task<CSharpContent>, name: string, value: string) =
        content.Map(fun c -> CSharpContent(Content.WithHeader name value c.AsContent))

    [<Extension>]
    static member WithContentType (content: Task<CSharpContent>, contentType: string) =
        content.Map(fun c -> CSharpContent(Content.WithContentType contentType c.AsContent))

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, headers: seq<Http.Header>) =
        content.Map(fun c -> CSharpContent(Content.SetHeaders headers c.AsContent))

    [<Extension>]
    static member SetHeaders (content: Task<CSharpContent>, [<ParamArray>] headers: Http.Header[]) =
        content.Map(fun c -> CSharpContent(Content.SetHeaders headers c.AsContent))

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, status: Http.Status) =
        content.Map(fun c -> CSharpContent(Content.SetStatus status c.AsContent))

    [<Extension>]
    static member SetStatus (content: Task<CSharpContent>, code: int, [<Optional>] message: string) =
        let status = (Http.Status.Custom code (CSharpContent.Opt message))
        content.Map(fun c -> CSharpContent(Content.SetStatus status c.AsContent))

    [<Extension>]
    static member SetBody (content: Task<CSharpContent>, writeBody: Action<Stream>) =
        content.Map(fun c -> CSharpContent(Content.SetBody writeBody.Invoke c.AsContent))

    static member RedirectPermanent (action: obj) =
        Content.RedirectPermanent action
        |> CSharpContent.ToContent

    static member RedirectPermanentToUrl (url: string) =
        Content.RedirectPermanentToUrl url
        |> CSharpContent.ToContent

    static member RedirectTemporary (action: obj) =
        Content.RedirectTemporary action
        |> CSharpContent.ToContent

    static member RedirectTemporaryToUrl (url: string) =
        Content.RedirectTemporaryToUrl url
        |> CSharpContent.ToContent

    static member Unauthorized() =
        Content.Unauthorized
        |> CSharpContent.ToContent

    static member Forbidden() =
        Content.Forbidden
        |> CSharpContent.ToContent

    static member NotFound() =
        Content.NotFound
        |> CSharpContent.ToContent

    static member ServerError() =
        Content.ServerError
        |> CSharpContent.ToContent

    static member MethodNotAllowed() =
        Content.MethodNotAllowed
        |> CSharpContent.ToContent
