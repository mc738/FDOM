// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions
open FDOM.Core.Parsing
open FDOM.Rendering

let getHeaderType (str : string) =
    
    let rec handler count (innerStr : char list) =
        match innerStr.[0] with
        | '#' -> handler (count + 1) innerStr.Tail
        | _ -> count
        
    let hType = handler 0 (str |> List.ofSeq)
    
    (hType, str.Substring(hType + 1))
    

[<EntryPoint>]
let main argv =

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
            .ParseLines(File.ReadAllLines("/home/max/Projects/FDOM/FDOM.IntegrationTests/test_article_1.md") |> List.ofArray)
            .CreateBlockContent()
    
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
    
    let html = Html.render layout stylesheets scripts doc  

    let timestamp = DateTime.Now.ToString("yyyyMMddHHmmss")
    File.WriteAllText($"/home/max/Data/FDOM_Tests/test_{timestamp}.html", html)
    
    //printfn "Result: %A" matches

    0 // return an integer exit code
