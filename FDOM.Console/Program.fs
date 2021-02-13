// Learn more about F# at http://fsharp.org

open System
open System.IO

[<RequireQualifiedAccess>]
type LineType =
    | Header
    | Text
    | OrderedListItem
    | UnorderedListItem
    | CodeBlockDelimited
    | Empty

type Line = {
    Number: int
    Type: LineType
    Text: string
}

let parse (lines: string seq) =
    lines
    |> List.ofSeq
    |> List.mapi (fun i l ->
        
        let text = l.Trim()
        
        let t =
            match text with
            | _ when String.IsNullOrEmpty(text) = true -> LineType.Empty
            | _ when text.Length < 3 -> LineType.Text // A guard to catch any short lines
            | _ when text.[0] = '#' -> LineType.Header
            | _ when text.[0..2] = @"```" -> LineType.CodeBlockDelimited
            | _ when text.[0] = '*' -> LineType.UnorderedListItem
            | _ when Char.IsDigit(text.[0]) && (text.[1] = '.' || text.[2] = '.') -> LineType.OrderedListItem // A bit of a hack to look for ordered lists. This can be cleaned up!
            | _ -> LineType.Text
        
        {
            Number = (i + 1)
            Type = t
            Text = text
        } : Line)

[<EntryPoint>]
let main argv =

    let lines = File.ReadAllLines("/home/max/Data/FDOM_Tests/articles/test.md")
    
    printfn "Map: %A" (parse lines)

    0 // return an integer exit code
