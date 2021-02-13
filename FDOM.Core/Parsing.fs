module FDOM.Core.Parsing

open System
open System.ComponentModel
open System.Text

module BlockParser =

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

    let cleanUp (lines: Line list) =
        let sb =
            // TODO Handle adding spaces after '.' and ','.
            lines
            |> List.fold (fun (sb: StringBuilder) l ->
                sb.Append(l.Text.Trim()) |> ignore
                sb) (StringBuilder())

        sb.ToString()

    let tryParseParagraph (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Text -> Error()
        | _ ->
            let (lines, next) =
                input.TryGetUntilNotTypeOrEnd(curr, LineType.Text)

            Ok(BlockToken.Paragraph(cleanUp lines), next)

    let tryParseHeaderBlock (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Header -> Error()
        | Some l -> Ok(BlockToken.Header l.Text, curr + 1)

    let tryParseCodeBlock (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.CodeBlockDelimited -> Error()
        | _ ->
            let (lines, next) =
                input.TryGetUntilTypeOrEnd(curr + 1, LineType.CodeBlockDelimited)

            Ok(BlockToken.CodeBlock(cleanUp lines), next)

    let tryParseOrderedListItem (input: Input) curr =
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
                Ok(BlockToken.OrderListItem(cleanUp (l :: lines)), next)
            | _ ->
                let (lines, next) =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr, LineType.Text)
                Ok(BlockToken.OrderListItem(cleanUp (l :: lines)), next)
                
    let tryParseUnorderedListItem (input: Input) curr =
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
                Ok(BlockToken.UnorderedListItem(cleanUp (l :: lines)), next)
            | _ ->
                let (lines, next) =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr, LineType.Text)
                Ok(BlockToken.UnorderedListItem(cleanUp (l :: lines)), next)

    let tryParseEmptyBlock (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Empty -> Error()
        | _ -> Ok(BlockToken.Empty, curr)
    
    let tryParseBlock (input: Input) curr =
        let result =
            [ tryParseOrderedListItem
              tryParseUnorderedListItem
              tryParseCodeBlock
              tryParseHeaderBlock
              tryParseParagraph
              tryParseEmptyBlock ]
            |> List.fold (fun state h ->
                match state with
                | Error _ -> h input curr
                | Ok r -> Ok r) (Error())

        match result with
        | Ok (token, i) -> Some(token, (i + 1))
        | Error _ -> None

    let parseBlocks input =
        let rec handler (state, i) =
            match tryParseBlock input i with
            | Some (token, next) -> handler (state @ [ token ], next)
            | None -> state

        handler ([], 0)
