// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Security.Cryptography
open System.Text.Json
open System.Text.RegularExpressions
open FDOM.Core.Parsing
open FDOM.Core.Styles
open FDOM.Core.Serialization
open FDOM.Rendering
open FDOM.Storage
open FLite.Core
open FDOM.Storage.Blobs


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

    //let qh = QueryHandler.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now:yyyyMMddHHmmss}.db")

    //let ds = DocumentStore(qh)

    //ds.Initialize()
    ()

[<EntryPoint>]
let main argv =

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
            .ParseLines(File.ReadAllLines("/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md")
                        |> List.ofArray)
            .CreateBlockContent()

    let doc : FDOM.Core.Common.DOM.Document =
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
                  Path = ""
                  VirtualPath = "css/main.css"
                  Type = "stylesheet" }
                { Name = "style_css"
                  Path = ""
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

    let stylesheets = doc.Resources
                      |> List.filter (fun r -> r.Type = "stylesheet")
                      |> List.map (fun r -> r.VirtualPath)
                      
    let scripts = doc.Resources
                  |> List.filter (fun r -> r.Type = "script")
                  |> List.map (fun r -> r.VirtualPath)
        
    let layout : Html.Layout =
        { Head = "<section id=\"sidebar\"><small>Main</small></section><main><small>Main</small>"
          Foot = "</main>" }

    let html =
        Html.render layout stylesheets scripts doc

    let renderedDocPath = $"/home/max/Data/FDOM_Tests/test_{DateTime.Now:yyyyMMddHHmmss}.html"
    
    File.WriteAllText(renderedDocPath, html)

    let qh = QueryHandler.Create($"/home/max/Data/FDOM_Tests/blob_store/{DateTime.Now:yyyyMMddHHmmss}.db")

    let ds = DocumentStore(qh)

    ds.Initialize()
    
    ds.AddDocument(doc, false, [ renderedDocPath ])
    
    //printfn "Result: %A" matches

    0 // return an integer exit code
