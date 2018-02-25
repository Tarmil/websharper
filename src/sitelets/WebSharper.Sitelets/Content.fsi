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

open System
open System.Threading.Tasks
open WebSharper

/// Represents the server response to an endpoint.
[<Struct; CompiledName "FSharpContent">]
type Content<'T> =
    | ConstantContent of constantResponse: Http.Response
    | CustomContent of response: (Context<'T> -> Http.Response)
    | CustomContentAsync of responseAsync: (Context<'T> -> Task<Http.Response>)

    /// Creates a JSON content from the given object.
    static member Json : 'U -> Content<'T>

    /// Creates an HTML content.
    static member Page
        : ?Body: #seq<#Web.INode>
        * ?Head: #seq<#Web.INode>
        * ?Title: string
        * ?Doctype: string
        -> Content<'T>

    /// Creates an HTML content.
    static member Page : Page -> Content<'T>

    /// Creates a plain text content.
    static member Text : string * ?encoding: System.Text.Encoding -> Content<'T>

    /// Creates a content that serves a file from disk.
    static member File : path: string * ?AllowOutsideRootFolder: bool * ?ContentType: string -> Content<'T>

    /// Creates a custom content.
    static member Custom : Http.Response -> Content<'T>

    /// Creates a custom content.
    static member Custom
        : ?Status: Http.Status
        * ?Headers: seq<Http.Header>
        * ?WriteBody: (System.IO.Stream -> unit)
        -> Content<'T>

/// Provides combinators for modifying content.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Content =

    /// Creates Content that depends on the Sitelet context.
    val FromContext : (Context<'T> -> Content<'T>) -> Content<'T>

    /// Generates an HTTP response.
    val ToResponse<'T> : Content<'T> -> Context<'T> -> Async<Http.Response>

    /// Generates an HTTP response.
    val ToResponseTask<'T> : Content<'T> -> Context<'T> -> Task<Http.Response>

    /// Wraps an asynchronous content.
    val FromAsync<'T> : Async<Content<'T>> -> Content<'T>

    /// Wraps an asynchronous content.
    val FromTask<'T> : Task<Content<'T>> -> Content<'T>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    val MapResponse<'T> : (Http.Response -> Http.Response) -> Content<'T> -> Content<'T>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    val MapResponseAsync<'T> : (Http.Response -> Async<Http.Response>) -> Content<'T> -> Content<'T>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    val WithHeaders<'T> : seq<Http.Header> -> Content<'T> -> Content<'T>

    /// Add a header to the generated response. Transforms any
    /// content to 'CustomContent'.
    val WithHeader<'T> : name: string -> value: string -> Content<'T> -> Content<'T>

    /// Set the Content-Type header.
    val WithContentType<'T> : string -> Content<'T> -> Content<'T>

    /// Replace the headers of the generated response. Transforms any
    /// content to 'CustomContent'.
    val SetHeaders<'T> : seq<Http.Header> -> Content<'T> -> Content<'T>

    /// Set the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    val SetStatus<'T> : status: Http.Status -> Content<'T> -> Content<'T>

    /// Set the body writing function of the generated response.
    /// Transforms any content to 'CustomContent'.
    val SetBody<'T> : writeBody: (System.IO.Stream -> unit) -> Content<'T> -> Content<'T>

    /// Redirects permanently (301 Moved Permanently) to a given action.
    val RedirectPermanent<'T> : action: 'T -> Content<'T>

    /// Redirects permanently (301 Moved Permanently) to a given URL.
    val RedirectPermanentToUrl : url: string -> Content<'T>

    /// Redirects temporarily (307 Redirect Temporary) to a given action.
    val RedirectTemporary<'T> : action: 'T -> Content<'T>

    /// Redirects temporarily (307 Redirect Temporary) to a given URL.
    val RedirectTemporaryToUrl : url: string -> Content<'T>

    /// Constructs a 401 Unauthorized response.
    val Unauthorized<'T> : Content<'T>

    /// Constructs a 403 Forbidden response.
    val Forbidden<'T> : Content<'T>

    /// Constructs a 404 Not Found response.
    val NotFound<'T> : Content<'T>

    /// Constructs a 501 Not Implemented response.
    val NotImplemented<'T> : Content<'T>

    /// Constructs a 500 Server Error response.
    val ServerError<'T> : Content<'T>

    /// Constructs a 405 Method Not Allowed response.
    val MethodNotAllowed<'T> : Content<'T>

    /// Constructs a 200 Ok response with empty body.
    val Ok<'T> : Content<'T>

    type RenderedResources =
        {
            Scripts : string
            Styles : string
            Meta : string
        }

        member Item : string -> string with get

[<Class>]
type ContentBuilder =
    member Bind : Async<'U> * ('U -> Content<'T>) -> Content<'T>
    member Bind : Task<'U> * ('U -> Content<'T>) -> Content<'T>
    member Return : Content<'T> -> Content<'T>
    member ReturnFrom : Content<'T> -> Content<'T>
    member Delay : f: (unit -> Content<'T>) -> Content<'T>
    member TryFinally : Content<'T> * (unit -> unit) -> Content<'T>
    member TryWith : Content<'T> * (exn -> Content<'T>) -> Content<'T>
// TODO:
//    member For : seq<'U> * ('U -> Content<'T>) -> Content<'T>
//    member While : (unit -> bool) * Content<'T> -> Content<'T>
//    member Zero : unit -> Content<'T>
//    member Using : 'U * ('U -> Content<'T>) -> Content<'T> when 'T :> IDisposable

[<AutoOpen>]
module ContentBuilder =
    val content : ContentBuilder

[<System.Runtime.CompilerServices.Extension; Sealed>]
type ContextExtensions =

    [<System.Runtime.CompilerServices.Extension>]
    static member GetSeparateResourcesAndScripts
        : this: Web.Context
        * controls: seq<#IRequiresResources>
        -> Content.RenderedResources

    [<System.Runtime.CompilerServices.Extension>]
    static member GetResourcesAndScripts
        : this: Web.Context
        * controls: seq<#IRequiresResources>
        -> string

open System.Runtime.InteropServices
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<CompiledName "Content"; Struct; Extension; NoEquality; NoComparison>]
type CSharpContent =

    val private c: Content<obj>

    member AsContent : Content<obj>

    /// Creates a JSON content from the given object.
    static member Json<'U>
        : 'U
        -> Task<CSharpContent>

    /// Creates an HTML content.
    static member Page
        : [<Optional>] Body: Web.INode
        * [<Optional>] Head: Web.INode
        * [<Optional>] Title: string
        * [<Optional>] Doctype: string
        -> Task<CSharpContent>

    /// Creates an HTML content.
    static member Page
        : Page
        -> Task<CSharpContent>

    /// Creates a plain text content.
    static member Text
        : string
        * [<Optional>] Encoding: System.Text.Encoding
        -> Task<CSharpContent>

    /// Creates a content that serves a file from disk.
    static member File
        : path: string
        * [<Optional>] AllowOutsideRootFolder: bool
        * [<Optional>] ContentType: string
        -> Task<CSharpContent>

    /// Creates a custom content.
    static member Custom
        : Http.Response
        -> Task<CSharpContent>

    /// Creates a custom content.
    static member Custom
        : [<Optional>] Status: Http.Status
        * [<Optional>] Headers: seq<Http.Header>
        * [<Optional>] WriteBody: Action<IO.Stream>
        -> Task<CSharpContent>

    /// Creates Content that depends on the Sitelet context.
    static member FromContext
        : Func<Context, Task<CSharpContent>>
        -> Task<CSharpContent>

    /// Generates an HTTP response.
    [<Extension>]
    static member ToResponse
        : CSharpContent
        * Context
        -> Task<Http.Response>

    /// Wraps an asynchronous content.
    static member FromTask
        : Task<CSharpContent>
        -> CSharpContent

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member MapResponse
        : Task<CSharpContent>
        * Func<Http.Response, Http.Response>
        -> Task<CSharpContent>

    /// Modify the response of a content. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member MapResponseAsync
        : Task<CSharpContent>
        * Func<Http.Response, Task<Http.Response>>
        -> Task<CSharpContent>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member WithHeaders
        : Task<CSharpContent>
        * seq<Http.Header>
        -> Task<CSharpContent>

    /// Add headers to the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member WithHeaders
        : Task<CSharpContent>
        * [<ParamArray>] headers: Http.Header[]
        -> Task<CSharpContent>

    /// Add a header to the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member WithHeader
        : Task<CSharpContent>
        * name: string
        * value: string
        -> Task<CSharpContent>

    /// Set the Content-Type header.
    [<Extension>]
    static member WithContentType
        : Task<CSharpContent>
        * contentType: string
        -> Task<CSharpContent>

    /// Replace the headers of the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member SetHeaders
        : Task<CSharpContent>
        * seq<Http.Header>
        -> Task<CSharpContent>

    /// Replace the headers of the generated response. Transforms any
    /// content to 'CustomContent'.
    [<Extension>]
    static member SetHeaders
        : Task<CSharpContent>
        * [<ParamArray>] headers: Http.Header[]
        -> Task<CSharpContent>

    /// Set the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    [<Extension>]
    static member SetStatus
        : Task<CSharpContent>
        * Http.Status
        -> Task<CSharpContent>

    /// Set the status of the generated response.
    /// Transforms any content to 'CustomContent'.
    [<Extension>]
    static member SetStatus
        : Task<CSharpContent>
        * code: int
        * [<Optional>] message: string
        -> Task<CSharpContent>

    /// Set the body writing function of the generated response.
    /// Transforms any content to 'CustomContent'.
    [<Extension>]
    static member SetBody
        : Task<CSharpContent>
        * Action<IO.Stream>
        -> Task<CSharpContent>

    /// Redirects permanently (301 Moved Permanently) to a given endpoint.
    static member RedirectPermanent
        : obj
        -> Task<CSharpContent>

    /// Redirects permanently (301 Moved Permanently) to a given URL.
    static member RedirectPermanentToUrl
        : string
        -> Task<CSharpContent>

    /// Redirects temporarily (307 Redirect Temporary) to a given endpoint.
    static member RedirectTemporary
        : obj
        -> Task<CSharpContent>

    /// Redirects temporarily (307 Redirect Temporary) to a given URL.
    static member RedirectTemporaryToUrl
        : string
        -> Task<CSharpContent>

    /// Constructs a 401 Unauthorized response.
    static member Unauthorized
        : unit
        -> Task<CSharpContent>

    /// Constructs a 403 Forbidden response.
    static member Forbidden
        : unit
        -> Task<CSharpContent>

    /// Constructs a 404 Not Found response.
    static member NotFound
        : unit
        -> Task<CSharpContent>

    /// Constructs a 500 Server Error response.
    static member ServerError
        : unit
        -> Task<CSharpContent>

    /// Constructs a 405 Method Not Allowed response.
    static member MethodNotAllowed
        : unit
        -> Task<CSharpContent>
