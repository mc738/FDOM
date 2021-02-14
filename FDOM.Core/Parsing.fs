module FDOM.Core.Parsing

open System
open System.ComponentModel
open System.Runtime.Serialization
open System.Text
open FDOM.Core.Common
open FDOM.Core.Common.Formatting

/// The block parse takes lines and parses it into block tokens.
module BlockParser =

    [<RequireQualifiedAccess>]
    type LineType =
        | Header
        | Text
        | OrderedListItem
        | UnorderedListItem
        | CodeBlockDelimited
        | Empty

    [<RequireQualifiedAccess>]
    type Line =
        { Number: int
          Text: string
          Type: LineType }

    let getLineType line =
        match line with
        | _ when String.IsNullOrEmpty(line) = true -> LineType.Empty
        | _ when line.Length < 3 -> LineType.Text // A guard to catch any short lines
        | _ when line.[0] = '#' -> LineType.Header
        | _ when line.[0..2] = @"```" -> LineType.CodeBlockDelimited
        | _ when line.[0] = '*' -> LineType.UnorderedListItem
        | _ when Char.IsDigit(line.[0])
                 && (line.[1] = '.' || line.[2] = '.') -> LineType.OrderedListItem // A bit of a hack to look for ordered lists. This can be cleaned up!
        | _ -> LineType.Text

    [<RequireQualifiedAccess>]
    type BlockToken =
        | Paragraph of string
        | Header of string
        | OrderListItem of string
        | UnorderedListItem of string
        | CodeBlock of string
        | Empty

    [<RequireQualifiedAccess>]
    type Input =
        { Lines: Line array }
        static member Create(lines: string list) =
            { Lines =
                  lines
                  |> List.mapi (fun i l ->
                      { Number = i + 1
                        Text = l
                        Type = getLineType l }: Line)
                  |> Array.ofList }

        member input.TryGetLine(index) =
            match index with
            | _ when index < 0 || index >= input.Lines.Length -> None
            | _ -> Some(input.Lines.[index])

        member input.LineCount = input.Lines.Length

        member input.TryGetUntilTypeOrEnd(curr, lineType) =
            let rec handler (state, i) =
                match input.TryGetLine i with
                | Some l when l.Type = lineType -> state, i
                | Some l -> handler (state @ [ l ], i + 1)
                | None -> state, input.LineCount - 1

            handler ([], curr)

        member input.TryGetUntilNotTypeOrEnd(curr, lineType) =
            let rec handler (state, i) =
                match input.TryGetLine i with
                | Some l when l.Type <> lineType -> state, i
                | Some l -> handler (state @ [ l ], i + 1)
                | None -> state, input.LineCount - 1

            handler ([], curr)


    let private formatBlockText (preprocessors: Formatters) (formatters: Formatters) (lines: Line list) =                
        let sb =
            lines
            |> List.fold (fun (sb: StringBuilder) l ->
                sb.Append(preprocessors.Run(l.Text)) |> ignore
                sb) (StringBuilder())

        formatters.Run(sb.ToString())

    let tryParseParagraph (formatter: Line list -> string) (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Text -> Error()
        | _ ->
            let (lines, next) =
                input.TryGetUntilNotTypeOrEnd(curr, LineType.Text)

            Ok(BlockToken.Paragraph(formatter lines), next)

    let tryParseHeaderBlock (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Header -> Error()
        | Some l -> Ok(BlockToken.Header l.Text, curr + 1)

    let tryParseCodeBlock (formatter: Line list -> string) (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.CodeBlockDelimited -> Error()
        | _ ->
            let (lines, next) =
                input.TryGetUntilTypeOrEnd(curr + 1, LineType.CodeBlockDelimited)

            Ok(BlockToken.CodeBlock(formatter lines), next)

    let tryParseOrderedListItem (formatter: Line list -> string) (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.OrderedListItem -> Error()
        | Some l ->
            // See what the next line is. if it's text handle accordingly.
            // TODO Refactor/clear up and make clear. Some voodoo going on.
            match input.TryGetLine (curr + 1) with   
            | Some next_line when next_line.Type = LineType.Text ->            
                let (lines, next) =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr + 1, LineType.Text)
                Ok(BlockToken.OrderListItem(formatter (l :: lines)), next)
            | _ ->
                let (lines, next) =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr, LineType.Text)
                Ok(BlockToken.OrderListItem(formatter (l :: lines)), next)
                
    let tryParseUnorderedListItem (formatter: Line list -> string) (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.UnorderedListItem -> Error()
        | Some l ->
            // See what the next line is. if it's text handle accordingly.
            // TODO Refactor/clear up and make clear. Some voodoo going on.
            match input.TryGetLine (curr + 1) with   
            | Some next_line when next_line.Type = LineType.Text ->            
                let (lines, next) =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr + 1, LineType.Text)
                Ok(BlockToken.UnorderedListItem(formatter (l :: lines)), next)
            | _ ->
                let (lines, next) =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr, LineType.Text)
                Ok(BlockToken.UnorderedListItem(formatter (l :: lines)), next)

    let tryParseEmptyBlock (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Empty -> Error()
        | _ -> Ok(BlockToken.Empty, curr)
    
    let tryParseBlock (input: Input) curr =
        
        let formatter = formatBlockText (Formatters.DefaultPreprocessors()) (Formatters.DefaultFormatters())
        
        let result =
            [ tryParseOrderedListItem formatter
              tryParseUnorderedListItem formatter
              tryParseCodeBlock formatter
              tryParseHeaderBlock
              tryParseParagraph formatter
              tryParseEmptyBlock ]
            |> List.fold (fun state h ->
                match state with
                | Error _ -> h input curr
                | Ok r -> Ok r) (Error())

        match result with
        | Ok (token, i) -> Some(token, (i + 1))
        | Error _ -> None



/// The inline parse takes block tokens and creates a DOM.
module InlineParser =
    
    
    let controlChars = [ '_'; '*'; '`' ]
    
    let isChar (chars: char list) c =
        chars
        |> List.fold (fun state curr ->
                match state with
                | true -> true
                | false -> curr = c 
            ) false
        
    let isControlChar = isChar controlChars
    
    let inBounds (str: string) i =
        match i with
        | _ when i > 0 || i <= str.Length -> false
        | _ -> true
    
    let getChar str i =
        match inBounds str i with
        | true -> Some(str.[i])
        | false -> None
    
    let readUntilChar input character from =
        let rec handler(i) =
            match getChar input i with
            | Some c when c = character -> i
            | Some _ -> handler(i + 1)
            | None -> input.Length - 1
            
        let endIndex = handler(from)
        
        input.[from..endIndex]
    
    
    let parseInlineContent (input: string) =
        
        
    
        
            
        
        DOM.InlineContent.Text  { Content = "" }
    
    
    
    
    let i = ()


let parseBlocks input =
    let rec handler (state, i) =
        match BlockParser.tryParseBlock input i with
        | Some (token, next) -> handler (state @ [ token ], next)
        | None -> state

    handler ([], 0)