// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
type LineType =
    | Header
    | Text
    | OrderedListItem
    | UnorderedListItem
    | CodeBlockDelimited
    | Empty

type Line =
    { Number: int
      Type: LineType
      Text: string }

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
            | _ when Char.IsDigit(text.[0])
                     && (text.[1] = '.' || text.[2] = '.') -> LineType.OrderedListItem // A bit of a hack to look for ordered lists. This can be cleaned up!
            | _ -> LineType.Text


        { Number = (i + 1)
          Type = t
          Text = text }: Line)

let objRegex (pattern: string) (input: string) (replacement: MatchEvaluator) =
    Regex.Replace(input, pattern, replacement)

let objRegexEvaluator =
    MatchEvaluator(fun m ->
        String.concat
            ""
            [ """<span class="special-fs">"""
              m.Value
              "</span>" ])

type Formatter =
    { Pattern: string
      Replacement: string }

    static member CreateFormatters(pattern, replacement) =
        { Pattern = pattern
          Replacement = replacement }

    member formatter.Run(input) =
        Regex.Replace(input, formatter.Pattern, formatter.Replacement)

type Formatters =
    { Items: Formatter list }
    static member Create(data: (string * string) list) =
        { Items =
              data
              |> List.map (fun (p, r) -> { Pattern = p; Replacement = r }) }

    member formatters.Run(input) = formatters.Items |> List.fold (fun state f -> f.Run(state) ) input

[<EntryPoint>]
let main argv =

    let formatters = Formatters.Create([
            "\,(?=[A-Za-z0-9])",", "
            "\.(?=[A-Za-z0-9])",". "
            "\!(?=[A-Za-z0-9])","! "
            "\?(?=[A-Za-z0-9])","? "
            "\%NOW\%", DateTime.Now.ToString("hh:mm dd MMM yy")
        ])

    
    let output = formatters.Run("Hello,world!This should get cleaned up.Hopefully it works?The time now is %NOW%.")
    

    let r =
        objRegex
            ("(?<![A-Za-z1-9])(Seq|List|Array|float|int)(?![A-Za-z1-9])")
            ("NewtonRahpson(f:float-&gt;float)(fDiff:float-&gt;float)(a0:float)")
            (objRegexEvaluator)


    //let lines = File.ReadAllLines("/home/max/Data/FDOM_Tests/articles/test.md")

    printfn "Result: %s" output

    0 // return an integer exit code
