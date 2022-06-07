// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Security.Cryptography
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.RegularExpressions
open FDOM.Core.Common
open FDOM.Core.Parsing
open FDOM.Core.Styles
open FDOM.Core.Serialization
open FDOM.Rendering
open FDOM.Storage
open FLite.Core
open FDOM.Storage.Blobs
open Fluff.Core


// TODO make tests.
let getHeaderType (str: string) =

    let rec handler count (innerStr: char list) =
        match innerStr.[0] with
        | '#' -> handler (count + 1) innerStr.Tail
        | _ -> count

    let hType = handler 0 (str |> List.ofSeq)

    (hType, str.Substring(hType + 1))

(*
let blobStoreTest =
    let qh =
        QueryHandler.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now}.db")

    let bs = BlobStore(qh, SHA256.Create())

    match bs.Initialize() with
    | Ok _ ->

        let blob1Ref =
            match bs.ImportFile("test", "/home/max/Pictures/live_server.png") with
            | Ok ref -> ref
            | Error _ -> failwith ""

        let blob2Ref =
            match bs.ImportFile("test", "/home/max/Pictures/R2CSeU4JMFB4QX2uiIDODJkqTgwXW67fhYDeLYpgvfI.webp") with
            | Ok ref -> ref
            | Error _ -> failwith ""

        let blob1R = bs.GetBlob(blob1Ref)
        let blob2R = bs.GetBlob(blob2Ref)

        printfn "Blob 1: %A" blob1R
        printfn "Blob 2: %A" blob2R

        blob1R.SaveAs("/home/max/Data/FDOM_Tests/blob_store/")
        |> ignore

        blob2R.SaveAs("/home/max/Data/FDOM_Tests/blob_store/")
        |> ignore

    | Error e -> printfn $"Error initializing blob store: '{e}'"
*)

let documentStoreTest =
    let blocks =
        Parser
            .ParseLines(
                File.ReadAllLines("C:\\Users\\44748\\Projects\FDOM\\FDOM.IntegrationTests\\test_article_1.md")
                |> List.ofArray
            )
            .CreateBlockContent()

    let doc: FDOM.Core.Common.DOM.Document =
        { Style = FDOM.Core.Common.DOM.Style.Default
          Name = "Test parsed document"
          Title = None
          Sections =
              [ { Style = FDOM.Core.Common.DOM.Style.Default
                  Title = None
                  Name = "Section 1"
                  Content = blocks } ]
          Resources =
              [ { Name = "main_css"
                  Path = "/home/max/Data/FDOM_Tests/css/style.css"
                  VirtualPath = "css/style.css"
                  Type = "stylesheet" }
                { Name = "style_css"
                  Path = "/home/max/Data/FDOM_Tests/css/main.css"
                  VirtualPath = "css/main.css"
                  Type = "stylesheet" }
                { Name = "index_js"
                  Path = "/home/max/Data/FDOM_Tests/js/index.js"
                  VirtualPath = "js/index.js"
                  Type = "script" }
                { Name = "main_js"
                  Path = "/home/max/Data/FDOM_Tests/js/main.js"
                  VirtualPath = "js/main.js"
                  Type = "script" }
                { Name = "original"
                  Path = "/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md"
                  VirtualPath = "articles/test_article_1.md"
                  Type = "artifact" }
                { Name = "image_1"
                  Path = "/home/max/Data/FDOM_Tests/images/live_server.png"
                  VirtualPath = "img/image_1.png"
                  Type = "image" }
                { Name = "image_2"
                  Path = "/home/max/Data/FDOM_Tests/images/R2CSeU4JMFB4QX2uiIDODJkqTgwXW67fhYDeLYpgvfI.webp"
                  VirtualPath = "img/image_2.webp"
                  Type = "image" } ] }
    //let qh = QueryHandler.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now:yyyyMMddHHmmss}.db")

    //let ds = DocumentStore(qh)
    //

    //ds.Initialize()
    ()

let pdfTest _ =
    let blocks =
        Parser
            .ParseLines(
                File.ReadAllLines("C:\\Users\\44748\\Projects\FDOM\\FDOM.IntegrationTests\\test_article_1.md")
                |> List.ofArray
            )
            .CreateBlockContent()

    let doc: FDOM.Core.Common.DOM.Document =
        { Style = FDOM.Core.Common.DOM.Style.Default
          Name = "Test parsed document"
          Title = None
          Sections =
              [ { Style = FDOM.Core.Common.DOM.Style.Default
                  Title = None
                  Name = "Section 1"
                  Content = blocks } ]
          Resources =
              [ { Name = "main_css"
                  Path = "/home/max/Data/FDOM_Tests/css/style.css"
                  VirtualPath = "css/style.css"
                  Type = "stylesheet" }
                { Name = "style_css"
                  Path = "/home/max/Data/FDOM_Tests/css/main.css"
                  VirtualPath = "css/main.css"
                  Type = "stylesheet" }
                { Name = "index_js"
                  Path = "/home/max/Data/FDOM_Tests/js/index.js"
                  VirtualPath = "js/index.js"
                  Type = "script" }
                { Name = "main_js"
                  Path = "/home/max/Data/FDOM_Tests/js/main.js"
                  VirtualPath = "js/main.js"
                  Type = "script" }
                { Name = "original"
                  Path = "/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md"
                  VirtualPath = "articles/test_article_1.md"
                  Type = "artifact" }
                { Name = "image_1"
                  Path = "/home/max/Data/FDOM_Tests/images/live_server.png"
                  VirtualPath = "img/image_1.png"
                  Type = "image" }
                { Name = "image_2"
                  Path = "/home/max/Data/FDOM_Tests/images/R2CSeU4JMFB4QX2uiIDODJkqTgwXW67fhYDeLYpgvfI.webp"
                  VirtualPath = "img/image_2.webp"
                  Type = "image" } ] }

    Pdf.render
        $"C:\\ProjectData\\TestPDFs\\{DateTime.Now.ToFileTime()}.pdf"
        "C:\\Users\\44748\\Projects\\PDFBuilder\\styles.json"
        doc

let mustasheTest _ =

    let template =
        File.ReadAllText("C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\page-template.mustache")

    let values =
        ({ Values =
               [ "title", Mustache.Value.Scalar "Examples"
                 "version", Mustache.Value.Scalar "v.0.1.0"
                 "thanks",
                 Mustache.Value.Scalar
                     """<p>Photo by <a href="https://unsplash.com/@emmafranceslogan?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Emma Frances Logan</a> on <a href="https://unsplash.com/s/photos/waiter?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a></p>"""
                 "now", Mustache.Value.Scalar(DateTime.Now.ToString("dd MMMM yyyy HH:mm:ss")) ]
               |> Map.ofList
           Partials = Map.empty }: Mustache.Data)

    let blocks =
        Parser
            .ParseLines(
                File.ReadAllLines("C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\examples.md")
                |> List.ofArray
            )
            .CreateBlockContent()

    let doc: FDOM.Core.Common.DOM.Document =
        { Style = FDOM.Core.Common.DOM.Style.Default
          Name = "Test parsed document"
          Title = None
          Sections =
              [ { Style = FDOM.Core.Common.DOM.Style.Default
                  Title = None
                  Name = "Section 1"
                  Content = blocks } ]
          Resources =
              [ { Name = "main_css"
                  Path = "/home/max/Data/FDOM_Tests/css/style.css"
                  VirtualPath = "css/style.css"
                  Type = "stylesheet" }
                { Name = "style_css"
                  Path = "/home/max/Data/FDOM_Tests/css/main.css"
                  VirtualPath = "css/main.css"
                  Type = "stylesheet" }
                { Name = "index_js"
                  Path = "/home/max/Data/FDOM_Tests/js/index.js"
                  VirtualPath = "js/index.js"
                  Type = "script" }
                { Name = "main_js"
                  Path = "/home/max/Data/FDOM_Tests/js/main.js"
                  VirtualPath = "js/main.js"
                  Type = "script" }
                { Name = "original"
                  Path = "/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md"
                  VirtualPath = "articles/test_article_1.md"
                  Type = "artifact" }
                { Name = "image_1"
                  Path = "/home/max/Data/FDOM_Tests/images/live_server.png"
                  VirtualPath = "img/image_1.png"
                  Type = "image" }
                { Name = "image_2"
                  Path = "/home/max/Data/FDOM_Tests/images/R2CSeU4JMFB4QX2uiIDODJkqTgwXW67fhYDeLYpgvfI.webp"
                  VirtualPath = "img/image_2.webp"
                  Type = "image" } ] }

    let result =
        Html.renderFromTemplate template values [] [] doc

    File.WriteAllText("C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\examples.html", result)

type DocumentAction =
    { DocumentPath: string
      Name: string
      NameSlug: string
      Thanks: string
      OutputPath: string }

let buildDocsFromTemplate (docs: DocumentAction list) (templatePath: string) =
    docs
    |> List.map
        (fun d ->
            let template = File.ReadAllText(templatePath)

            let values =
                ({ Values =
                       [ "title", Mustache.Value.Scalar d.Name
                         "titleSlug", Mustache.Value.Scalar d.NameSlug
                         "version", Mustache.Value.Scalar "v.0.1.0"
                         "thanks", Mustache.Value.Scalar d.Thanks
                         "now", Mustache.Value.Scalar(DateTime.Now.ToString("dd MMMM yyyy HH:mm:ss")) ]
                       |> Map.ofList
                   Partials = Map.empty }: Mustache.Data)

            let blocks =
                Parser
                    .ParseLines(File.ReadAllLines(d.DocumentPath) |> List.ofArray)
                    .CreateBlockContent()

            let doc: FDOM.Core.Common.DOM.Document =
                { Style = FDOM.Core.Common.DOM.Style.Default
                  Name = "Test parsed document"
                  Title = None
                  Sections =
                      [ { Style = FDOM.Core.Common.DOM.Style.Default
                          Title = None
                          Name = "Section 1"
                          Content = blocks } ]
                  Resources =
                      [ { Name = "main_css"
                          Path = "/home/max/Data/FDOM_Tests/css/style.css"
                          VirtualPath = "css/style.css"
                          Type = "stylesheet" }
                        { Name = "style_css"
                          Path = "/home/max/Data/FDOM_Tests/css/main.css"
                          VirtualPath = "css/main.css"
                          Type = "stylesheet" }
                        { Name = "index_js"
                          Path = "/home/max/Data/FDOM_Tests/js/index.js"
                          VirtualPath = "js/index.js"
                          Type = "script" }
                        { Name = "main_js"
                          Path = "/home/max/Data/FDOM_Tests/js/main.js"
                          VirtualPath = "js/main.js"
                          Type = "script" }
                        { Name = "original"
                          Path = "/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md"
                          VirtualPath = "articles/test_article_1.md"
                          Type = "artifact" }
                        { Name = "image_1"
                          Path = "/home/max/Data/FDOM_Tests/images/live_server.png"
                          VirtualPath = "img/image_1.png"
                          Type = "image" }
                        { Name = "image_2"
                          Path = "/home/max/Data/FDOM_Tests/images/R2CSeU4JMFB4QX2uiIDODJkqTgwXW67fhYDeLYpgvfI.webp"
                          VirtualPath = "img/image_2.webp"
                          Type = "image" } ] }

            let result =
                Html.renderFromTemplate template values [] [] doc

            File.WriteAllText(d.OutputPath, result))

// "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\page-template.mustache"

// "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\examples.html"
(*
waiter values:
title - page title
version - page version
content - page content
thanks - path thanks
now - datetime now
*)


module HtmlPipeline =
        
    type BuildAction =
        { [<JsonPropertyName("documentPath")>]
          DocumentPath: string
          [<JsonPropertyName("name")>]
          Name: string
          [<JsonPropertyName("nameSlug")>]
          NameSlug: string
          [<JsonPropertyName("thanks")>]
          Thanks: string
          [<JsonPropertyName("outputPath")>]
          OutputPath: string }

    type DocumentGroup =
        { [<JsonPropertyName("templatePath")>]
          TemplatePath: string
          [<JsonPropertyName("buildActions")>]
          BuildActions: BuildAction list }
        
    type PipelineConfig = {
        [<JsonPropertyName("actions")>]
        Actions: DocumentGroup seq
    }
    
    let loadConfig (path: string) =
        try
            File.ReadAllText path |> JsonSerializer.Deserialize<PipelineConfig> |> Ok
        with
        | exn -> Error exn.Message
        
    let run (config: PipelineConfig) =
        config.Actions
        |> List.ofSeq
        |> List.map (fun dg ->
            dg.BuildActions
            |> List.map (fun ba ->
                ({
                    DocumentPath = ba.DocumentPath
                    Name = ba.Name
                    NameSlug = ba.NameSlug
                    Thanks = ba.Thanks
                    OutputPath = ba.OutputPath
                }: DocumentAction))
            |> buildDocsFromTemplate
            <| dg.TemplatePath)



[<EntryPoint>]
let main argv =



    "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\page-template.mustache"
    |> buildDocsFromTemplate [ { DocumentPath =
                                     "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\documentation\\examples.md"
                                 Name = "Examples"
                                 NameSlug = "examples"
                                 Thanks =
                                     """Photo by <a href="https://unsplash.com/@johnschno?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">John Schnobrich</a> on <a href="https://unsplash.com/s/photos/work-together?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>"""
                                 OutputPath = "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\examples.html" }
                               { DocumentPath =
                                     "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\documentation\\commands.md"
                                 Name = "Commands"
                                 NameSlug = "commands"
                                 Thanks =
                                     """Photo by <a href="https://unsplash.com/@cdr6934?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Chris Ried</a> on <a href="https://unsplash.com/s/photos/code?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>"""
                                 OutputPath = "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\commands.html" }
                               { DocumentPath =
                                     "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\documentation\\jobs.md"
                                 Name = "Jobs"
                                 NameSlug = "jobs"
                                 Thanks =
                                     """Photo by <a href="https://unsplash.com/@jramos10?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Josue Isai Ramos Figueroa</a> on <a href="https://unsplash.com/s/photos/construction?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>"""
                                 OutputPath = "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\jobs.html" }
                               { DocumentPath =
                                     "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\documentation\\overview.md"
                                 Name = "Overview"
                                 NameSlug = "overview"
                                 Thanks =
                                     """Photo by <a href="https://unsplash.com/@carolineattwood?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Caroline Attwood</a> on <a href="https://unsplash.com/s/photos/waiter?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>"""
                                 OutputPath = "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\overview.html" }
                               { DocumentPath =
                                     "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\documentation\\routes.md"
                                 Name = "Routes"
                                 NameSlug = "routes"
                                 Thanks =
                                     """Photo by <a href="https://unsplash.com/@ed259?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Ed 259</a> on <a href="https://unsplash.com/s/photos/roads?utm_source=unsplash&utm_medium=referral&utm_content=creditCopyText">Unsplash</a>"""
                                 OutputPath = "C:\\Users\\44748\\Projects\\__prototypes\\waiter_website\\routes.html" } ]
    |> ignore

    //mustasheTest ()
    pdfTest ()
    // auditTest
    // documentStoreTest

    // blobStoreTest

    let namedStyle = NamedStyle.Example.GetSelector()

    let (count1, result1) = getHeaderType "# Hello, World!"
    let (count2, result2) = getHeaderType "## Hello, World!"
    let (count3, result3) = getHeaderType "### Hello, World!"
    let (count4, result4) = getHeaderType "#### Hello, World!"
    let (count5, result5) = getHeaderType "##### Hello, World!"
    let (count6, result6) = getHeaderType "###### Hello, World!"

    printfn $"Result 1: '{result1}', HeaderType: {count1}"
    printfn $"Result 2: '{result2}', HeaderType: {count2}"
    printfn $"Result 3: '{result3}', HeaderType: {count3}"
    printfn $"Result 4: '{result4}', HeaderType: {count4}"
    printfn $"Result 5: '{result5}', HeaderType: {count5}"
    printfn $"Result 6: '{result6}', HeaderType: {count6}"


    let blocks =
        Parser
            .ParseLines(
                File.ReadAllLines("/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md")
                |> List.ofArray
            )
            .CreateBlockContent()

    let doc: FDOM.Core.Common.DOM.Document =
        { Style = FDOM.Core.Common.DOM.Style.Default
          Name = "Test parsed document"
          Title = None
          Sections =
              [ { Style = FDOM.Core.Common.DOM.Style.Default
                  Title = None
                  Name = "Section 1"
                  Content = blocks } ]
          Resources =
              [ { Name = "main_css"
                  Path = "/home/max/Data/FDOM_Tests/css/style.css"
                  VirtualPath = "css/style.css"
                  Type = "stylesheet" }
                { Name = "style_css"
                  Path = "/home/max/Data/FDOM_Tests/css/main.css"
                  VirtualPath = "css/main.css"
                  Type = "stylesheet" }
                { Name = "index_js"
                  Path = "/home/max/Data/FDOM_Tests/js/index.js"
                  VirtualPath = "js/index.js"
                  Type = "script" }
                { Name = "main_js"
                  Path = "/home/max/Data/FDOM_Tests/js/main.js"
                  VirtualPath = "js/main.js"
                  Type = "script" }
                { Name = "original"
                  Path = "/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md"
                  VirtualPath = "articles/test_article_1.md"
                  Type = "artifact" }
                { Name = "image_1"
                  Path = "/home/max/Data/FDOM_Tests/images/live_server.png"
                  VirtualPath = "img/image_1.png"
                  Type = "image" }
                { Name = "image_2"
                  Path = "/home/max/Data/FDOM_Tests/images/R2CSeU4JMFB4QX2uiIDODJkqTgwXW67fhYDeLYpgvfI.webp"
                  VirtualPath = "img/image_2.webp"
                  Type = "image" } ] }

    //Serializer.WriteToFile(doc, "/home/max/Data/FDOM_Tests/test.json")

    //let t =
    //    Serializer.Deserialize(File.ReadAllText("/home/max/Data/FDOM_Tests/test.json"))

    //printfn "Test: %A" t

    let stylesheets =
        doc.Resources
        |> List.filter (fun r -> r.Type = "stylesheet")
        |> List.map (fun r -> $"{doc.SnakeCaseName}/{r.VirtualPath}")

    let scripts =
        doc.Resources
        |> List.filter (fun r -> r.Type = "script")
        |> List.map (fun r -> $"{doc.SnakeCaseName}/{r.VirtualPath}")

    let layout: Html.Layout =
        { Head = "<section id=\"sidebar\"><small>Main</small></section><main><small>Main</small>"
          Foot = "</main>" }

    let html =
        Html.render layout stylesheets scripts doc

    let renderedDocPath =
        $"/home/max/Data/FDOM_Tests/test_{DateTime.Now:yyyyMMddHHmmss}.html"

    File.WriteAllText(renderedDocPath, html)

    //let qh = QueryHandler.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now:yyyyMMddHHmmss}.db")

    let ds =
        DocumentStore.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now:yyyyMMddHHmmss}.db")

    let rendererDocs: DOM.RenderedDocument list =
        [ { Path = renderedDocPath
            VirtualPath = "index.html" } ]

    let docRef = ds.AddDocument(doc, false, rendererDocs)

    //ds.AddDocumentVersion(docRef, doc, 1, 1, 0, "test", [ renderedDocPath ]) |> ignore

    0 // return an integer exit code
