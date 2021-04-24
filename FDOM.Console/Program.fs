// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions
open FDOM.Core.Parsing
open FDOM.Rendering

[<EntryPoint>]
let main argv =

    let blocks =
        Parser
            .ParseLines(File.ReadAllLines("/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md") |> List.ofArray)
            .CreateBlockContent()

    printfn "Blocks: %A" blocks

    
    let doc : FDOM.Core.Common.DOM.Document =
        { Style = FDOM.Core.Common.DOM.Style.Default
          Name = "Test parsed document"
          Title = None
          Sections =
              [ { Style = FDOM.Core.Common.DOM.Style.Default
                  Title = None
                  Name = "Section 1"
                  Content = blocks } ] }

    let stylesheets = [
        "css/main.css"
        "css/style.css"
    ]
    
    let scripts = [
        "js/index.js"
        "js/main.js"
    ]
    
    let layout = {
        Head = "<section id=\"sidebar\"><small>Main</small></section><main><small>Main</small>"
        Foot = "</main>" } : Html.Layout
    
    printfn "Doc: %A" doc
    
    let html = Html.render layout stylesheets scripts doc  

    let timestamp = DateTime.Now.ToString("yyyyMMddHHmmss")
    File.WriteAllText($"/home/max/Data/FDOM_Tests/test_{timestamp}.html", html)
    
    //printfn "Result: %A" matches

    0 // return an integer exit code
