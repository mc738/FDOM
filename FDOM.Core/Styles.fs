module FDOM.Core.Styles

open System.Text

type NamedStyle =
    | CodeBlock
    | InlineCodeBlock
    | StandOut
    | Example
    
    member namedStyle.GetSelector() =
        match namedStyle with
        | CodeBlock -> "code-block"
        | InlineCodeBlock -> "code"
        | StandOut -> "stand-out"
        | Example -> "example"
            
  
type SelectorType =
    | Named of NamedStyle
    | Verbatim of string 

    member selector.GetSelector() =
        match selector with
        | Named ns -> ns.GetSelector()
        | Verbatim s -> s

type StyleBlock =
    { Selector: SelectorType
      KeyValues: Map<string, string> }
    member block.ToCss(sb: StringBuilder) =
        sb.AppendLine $"%s{block.Selector.GetSelector()} {{" |> ignore

        block.KeyValues
        |> Map.map (fun k v -> sb.AppendLine($"    {k}: {v};"))
        |> ignore

        sb

type StyleSheet =
    { Blocks: StyleBlock list }
    member sheet.ToCss() =
        let sb = StringBuilder()

        sheet.Blocks
        |> List.map (fun b -> b.ToCss(sb))
        |> ignore

        sb.ToString()
