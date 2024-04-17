module FDOM.Core.Parsing

open System
open System.Runtime.Serialization
open System.Text
open System.Text.RegularExpressions
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
        | Image
        | Table
        | InlineMetadata
        | Footnote
        | IndentedText
        | BlockQuote
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
        | _ when Char.IsDigit(line.[0]) && (line.[1] = '.' || line.[2] = '.') -> LineType.OrderedListItem // A bit of a hack to look for ordered lists. This can be cleaned up!
        | _ when line.[0] = '!' -> LineType.Image
        | _ when line.[0] = '|' -> LineType.Table
        | _ when line.[0] = '#' && line.[1] = '{' -> LineType.InlineMetadata
        | _ when line.[0] = '>' -> LineType.BlockQuote
        | _ when line.[0] = ' ' || line.[0] = '\t' -> LineType.IndentedText 
        | _ -> LineType.Text

    [<RequireQualifiedAccess>]
    type BlockToken =
        | Paragraph of Text: string
        | Header of Text: string
        | OrderListItem of Text: string
        | UnorderedListItem of Text: string
        | CodeBlock of Language: string option * Text: string
        | Image of Text: string
        | Table of Text: string
        | InlineMetadata of Text: string
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
                      Type = getLineType l }
                    : Line)
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
            
        member input.TryGetUntilNotTypesOrEnd(curr, lineTypes: LineType list) =
            let rec handler (state, i) =
                match input.TryGetLine i with
                | Some l when lineTypes |> List.contains l.Type |> not -> state, i
                | Some l -> handler (state @ [ l ], i + 1)
                | None -> state, input.LineCount - 1

            handler ([], curr)

    let private formatBlockText (preprocessors: Formatters) (formatters: Formatters) (lines: Line list) =
        let sb =
            lines
            |> List.fold
                (fun (sb: StringBuilder) l ->
                    sb.Append(preprocessors.Run(l.Text)) |> ignore
                    sb)
                (StringBuilder())

        formatters.Run(sb.ToString())

    let tryParseParagraph (formatter: Line list -> string) (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Text -> Error()
        | _ ->
            // This looks for text or indented text because paragraph lines could start with a space.
            let lines, next = input.TryGetUntilNotTypesOrEnd(curr, [ LineType.Text; LineType.IndentedText ])

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
        | Some l ->
            // TODO tidy up.
            let lang =
                l.Text.Replace("`", "").Trim()
                |> fun l ->
                    match String.IsNullOrWhiteSpace l with
                    | true -> None
                    | false -> Some l


            let lines, next = input.TryGetUntilTypeOrEnd(curr + 1, LineType.CodeBlockDelimited)

            // Special formatter to preserve line breaks.
            Ok(
                BlockToken.CodeBlock(lang, lines |> List.map (fun l -> l.Text) |> String.concat Environment.NewLine),
                next
            )

    let tryParseOrderedListItem (formatter: Line list -> string) (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.OrderedListItem -> Error()
        | Some l ->
            // See what the next line is. if it's text handle accordingly.
            // TODO Refactor/clear up and make clear. Some voodoo going on.
            match input.TryGetLine(curr + 1) with
            | Some next_line when next_line.Type = LineType.Text ->
                let lines, next =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr + 1, LineType.Text)

                Ok(BlockToken.OrderListItem(formatter (l :: lines)), next)
            | _ ->
                let lines, next =
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
            match input.TryGetLine(curr + 1) with
            | Some next_line when next_line.Type = LineType.Text ->
                let lines, next =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr + 1, LineType.Text)

                Ok(BlockToken.UnorderedListItem(formatter (l :: lines)), next)
            | _ ->
                let lines, next =
                    // Look from next line on. we already know this line will be a list item
                    input.TryGetUntilNotTypeOrEnd(curr, LineType.Text)

                Ok(BlockToken.UnorderedListItem(formatter (l :: lines)), next)

    let tryParseImage (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Image -> Error()
        | Some l -> Ok(BlockToken.Image l.Text, curr)

    let tryParseTable (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Table -> Error()
        | Some l -> Ok(BlockToken.Table l.Text, curr)

    let tryParseInlineMetadata (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.InlineMetadata -> Error()
        | Some l -> Ok(BlockToken.InlineMetadata l.Text, curr)

    let tryParseFootnote (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Footnote -> Error()
    
    let tryParseBlockQuote (input: Input) curr =
        
        ()
     
    let tryParseEmptyBlock (input: Input) curr =
        match input.TryGetLine curr with
        | None -> Error()
        | Some l when l.Type <> LineType.Empty -> Error()
        | _ -> Ok(BlockToken.Empty, curr)

    let tryParseBlock (input: Input) curr =

        let formatter =
            formatBlockText (Formatters.DefaultPreprocessors()) (Formatters.DefaultFormatters())

        let result =
            [ tryParseOrderedListItem formatter
              tryParseUnorderedListItem formatter
              tryParseCodeBlock formatter
              tryParseHeaderBlock
              tryParseParagraph formatter
              tryParseImage
              tryParseTable
              tryParseInlineMetadata
              tryParseEmptyBlock ]
            |> List.fold
                (fun state h ->
                    match state with
                    | Error _ -> h input curr
                    | Ok r -> Ok r)
                (Error())

        match result with
        | Ok(token, i) -> Some(token, (i + 1))
        | Error _ -> None

    let parseBlocks input =
        let rec handler (state, i) =
            match tryParseBlock input i with
            | Some(token, next) -> handler (state @ [ token ], next)
            | None -> state

        handler ([], 0)

/// The inline parse takes block tokens and creates a DOM.
module InlineParser =

    let controlChars = [ '_'; '*'; '`'; '[' ]

    let isChar (chars: char list) c =
        chars
        |> List.fold
            (fun state curr ->
                match state with
                | true -> true
                | false -> curr = c)
            false

    let isControlChar = isChar controlChars

    let inBounds (str: string) i =
        match i with
        | _ when i < 0 || i >= str.Length -> false
        | _ -> true

    let getChar str i =
        match inBounds str i with
        | true -> Some(str.[i])
        | false -> None

    let lookAhead str i count = getChar str (i + count)

    let lookBack str i count = getChar str (i - count)

    let compareLookAhead str (pattern: string) i =
        match i > 0, inBounds str (i + pattern.Length) with
        | true, true ->
            let s = str.[i .. (i + pattern.Length - 1)]
            s = pattern
        | _ -> false

    /// Read from a index until a specified character.
    /// This returns the sub string and either index of the character of the next index alone if `inclusive is set to true`.
    let readUntilChar input character inclusive from =
        let rec handler i =
            match getChar input i with
            | Some c when c = character -> i
            | Some _ -> handler (i + 1)
            | None -> input.Length

        let endIndex = handler from

        (input.[from .. (endIndex - 1)], (if inclusive then endIndex + 1 else endIndex))

    let readUntilCtrlChar input from =
        let rec handler i =
            match getChar input i with
            | Some c when isControlChar c -> i
            | Some _ -> handler (i + 1)
            | None -> input.Length

        let endIndex = handler from

        (input.[from .. (endIndex - 1)], endIndex)

    /// Read from a index until a string.
    /// This returns the sub string and either index of the character of the next index alone if `inclusive is set to true`.
    let readUntilString input (pattern: string) inclusive from =
        let peek = compareLookAhead input pattern

        let rec handler i =
            match inBounds input (pattern.Length + i) with
            | true ->
                match peek i with
                | true -> i
                | false -> handler (i + 1)
            | false -> input.Length

        let endIndex = handler (from + pattern.Length - 1)

        (input.[(from + pattern.Length) .. (endIndex - 1)],
         if inclusive then (endIndex + pattern.Length) else endIndex)

    //type

    let parseInlineContent (input: string) =

        // Cycle through input,
        // ` is completely delimited (

        let rec handler (state, i) =
            match getChar input i with
            | Some c ->
                //
                let newState, next =
                    match c with
                    | '*' ->
                        // Fix for issue #4 - if `next` is the end of input, remove `len` from the end of `str`
                        // This sorts issues where `***Hello world***` would parse as `Hello world***`
                        // if at the end of input.
                        let trimEnd (len: int) (str: string, next: int) =
                            if (next >= input.Length) then
                                str.Remove(str.Length - len, len), next
                            else
                                str, next

                        let (sub, next), classes =
                            match lookAhead input i 1, lookAhead input i 2 with
                            | Some c1, Some c2 when c1 = '*' && c2 = '*' ->
                                readUntilString input "***" true i |> trimEnd 3,
                                [ PredefinedStyleRefs.bold; PredefinedStyleRefs.italics ]
                            | Some c1, _ when c1 = '*' ->
                                readUntilString input "**" true i |> trimEnd 2, [ PredefinedStyleRefs.bold ]
                            | _ -> readUntilChar input '*' true (i + 1), [ PredefinedStyleRefs.italics ]

                        let content =
                            DOM.InlineContent.Span
                                { Content = sub
                                  Style = DOM.Style.Ref classes }

                        (state @ [ content ], next)
                    | '`' ->
                        let sub, next = readUntilChar input '`' true (i + 1)
                        // Make this a span
                        let content =
                            DOM.InlineContent.Span
                                { Content = sub
                                  Style = DOM.Style.Ref [ PredefinedStyleRefs.code ] }

                        (state @ [ content ], next)
                    | '[' ->
                        // '[' can either be a link or foot not

                        let text, next = readUntilChar input ']' true (i + 1)

                        match text.StartsWith('^') with
                        | true ->
                            let content = DOM.InlineContent.FootnoteReference <| text.Remove(0, 1)

                            (state @ [ content ], next)
                        | false ->
                            // If thr
                            let url, next = readUntilChar input ')' true (next + 1)

                            let content =
                                DOM.InlineContent.Link
                                    { Content = text
                                      Url = url
                                      Style = DOM.Style.Default }

                            (state @ [ content ], next)
                    | '_' ->
                        // To fix issue #8 and #9
                        // Read until next control character and append to prev?
                        let sub, next = readUntilCtrlChar input (i + 1)

                        // Get last item from state and append "_" + sub to it.
                        // Fix for #8 and #9
                        let newIc =
                            state
                            |> List.rev
                            |> List.tryHead
                            |> Option.map (fun ic -> ic.Append($"_{sub}"))
                            |> Option.defaultWith (fun _ -> DOM.InlineContent.Text { Content = $"_{sub}" })

                        // Not the prettiest solution but it does get the job done.
                        // Could be more efficient but in general this *shouldn't* be too much of an issue.
                        // It passes tests for now. This could be reworked properly.
                        (state |> List.rev |> List.tail |> (fun t -> newIc :: t |> List.rev, next))
                    | _ ->
                        let sub, next = readUntilCtrlChar input i

                        let content = DOM.InlineContent.Text { Content = sub }

                        (state @ [ content ], next)

                handler (newState, next)
            | None -> state

        handler ([], 0)

module Processing =

    open FDOM.Core.Dsl
    open FDOM.Core.Dsl.General

    let getHeaderType (str: string) =

        let rec handler count (innerStr: char list) =
            match innerStr.[0] with
            | '#' -> handler (count + 1) innerStr.Tail
            | _ -> count

        let hType = handler 0 (str |> List.ofSeq)

        (hType, str.Substring(hType + 1))

    let createHeaderContent (value: string) =
        let hType, content = getHeaderType value

        match hType with
        | 1 -> h1 true Style.none (InlineParser.parseInlineContent content)
        | 2 -> h2 false Style.none (InlineParser.parseInlineContent content)
        | 3 -> h3 false Style.none (InlineParser.parseInlineContent content)
        | 4 -> h4 false Style.none (InlineParser.parseInlineContent content)
        | 5 -> h5 false Style.none (InlineParser.parseInlineContent content)
        | 6 -> h6 false Style.none (InlineParser.parseInlineContent content)
        | _ -> h6 false Style.none (InlineParser.parseInlineContent content)

    let createParagraphContent (value: string) =
        p Style.none (InlineParser.parseInlineContent value)

    let createCodeBlock (lang: string option) (value: string) =
        code
            (Style.references [ lang |> Option.map (fun l -> $"language-{l}") |> Option.defaultValue "" ])
            [ DOM.InlineContent.Text { Content = value } ]


    let createImageBlock (value: string) =
        // Split the image into parts.

        // For example ![alt text](url "image title"){ height:[height],width:[width] }

        let altText, next = InlineParser.readUntilString value "]" false 1

        let url, next = InlineParser.readUntilChar value ' ' false (next + 2)

        let title, next = InlineParser.readUntilChar value '"' false (next + 2)

        let hw, _ = InlineParser.readUntilChar value '}' false (next + 3)

        let height, width =
            hw.Trim().Split(',')
            |> Array.fold
                (fun (h, w) v ->
                    let s = v.Trim().Split(':')

                    match s.Length > 1, s.[0].ToLower() with
                    | true, "height" -> Some s.[1], w
                    | true, "width" -> h, Some s.[1]
                    | _ -> h, w)
                (None, None)

        img Style.none url title altText height width


    let createListItem (value: string) =
        li Style.none (InlineParser.parseInlineContent value)

    let createOrderedListItems (values: string list) =
        ol Style.none (values |> List.map createListItem)

    let createUnorderedListItem (values: string list) =
        ul Style.none (values |> List.map createListItem)

    let createTable (values: string list) =

        // Clean up the line to remove leading and possible trailing '|' characters.
        let cleanLine (l: string) =
            match l.EndsWith('|') with
            | true -> l.Remove(l.Length - 1, 1).Remove(0, 1)
            | false -> l.Remove(0, 1)

        let splitLine (l: string) =
            // TODO handle delimited |'s, e.g. in backticks.
            l.Split('|', StringSplitOptions.TrimEntries)

        // TODO add check/handling for 2nd line containing ----

        let createColumns (line1: string) (line2: string option) =
            line1
            |> cleanLine
            |> splitLine
            |> List.ofArray
            |> List.mapi (fun i l ->
                ({ Style = Style.none
                   Content = InlineParser.parseInlineContent l
                   Alignment = DOM.TableColumnAlignment.Left // TODO handle
                   Index = i }
                : DOM.TableColumn))

        // First check if the table has at least 2 lines
        match values.Length with
        | 0 ->
            DOM.BlockContent.Table
                { Style = Style.none
                  Columns = []
                  Rows = [] }
        | 1 ->
            DOM.BlockContent.Table
                { Style = Style.none
                  Columns = createColumns values.[0] None
                  Rows = [] }
        | 2 ->
            DOM.BlockContent.Table
                { Style = Style.none
                  Columns = createColumns values.[0] (Some values.[1])
                  Rows = [] }
        | _ ->
            let _, rows = values |> List.splitAt 2

            let createRow (line: string) =
                ({ Style = Style.none
                   Cells =
                     cleanLine line
                     |> splitLine
                     |> List.ofArray
                     |> List.mapi (fun i c ->
                         ({ Style = Style.none
                            ColumnIndex = i
                            Content = InlineParser.parseInlineContent c }
                         : DOM.TableCell)) }
                : DOM.TableRow)

            DOM.BlockContent.Table
                { Style = Style.none
                  Columns = createColumns values.[0] (Some values.[1])
                  Rows = rows |> List.map createRow }

    let rec collectOrderedListItems (collected: string list) (remaining: BlockParser.BlockToken list) =
        match remaining |> List.tryHead with
        | Some item ->
            match item with
            | BlockParser.BlockToken.OrderListItem v -> collectOrderedListItems (collected @ [ v ]) remaining.Tail
            | _ -> (collected, remaining)
        | None -> (collected, remaining)

    let rec collectUnorderedListItems (collected: string list) (remaining: BlockParser.BlockToken list) =
        match remaining |> List.tryHead with
        | Some item ->
            match item with
            | BlockParser.BlockToken.UnorderedListItem v -> collectUnorderedListItems (collected @ [ v ]) remaining.Tail
            | _ -> (collected, remaining)
        | None -> (collected, remaining)

    let rec collectTableLines (collected: string list) (remaining: BlockParser.BlockToken list) =
        match remaining |> List.tryHead with
        | Some item ->
            match item with
            | BlockParser.BlockToken.Table v -> collectTableLines (collected @ [ v ]) remaining.Tail
            | _ -> (collected, remaining)
        | None -> (collected, remaining)

    let private append (blocks: DOM.BlockContent list) block = blocks @ [ block ]

    /// Recursively process `BlockToken`s into `BlockContent`.
    let processBlocks (blocks: BlockParser.BlockToken list) =
        let rec handler (processedBlocks: DOM.BlockContent list, remainingBlocks: BlockParser.BlockToken list) =
            match remainingBlocks.IsEmpty with
            | true -> processedBlocks
            | false ->
                let newBlock, newRemainingBlocks =
                    match remainingBlocks.Head with
                    | BlockParser.BlockToken.Header h -> (createHeaderContent h, remainingBlocks.Tail)
                    | BlockParser.BlockToken.Paragraph p -> (createParagraphContent p, remainingBlocks.Tail)
                    | BlockParser.BlockToken.CodeBlock(lang, c) -> (createCodeBlock lang c, remainingBlocks.Tail)
                    | BlockParser.BlockToken.OrderListItem _ ->
                        let collected, remaining = collectOrderedListItems [] remainingBlocks

                        (createOrderedListItems collected, remaining)
                    | BlockParser.BlockToken.UnorderedListItem _ ->
                        let collected, remaining = collectUnorderedListItems [] remainingBlocks

                        (createUnorderedListItem collected, remaining)

                    | BlockParser.BlockToken.Image v -> createImageBlock v, remainingBlocks.Tail
                    | BlockParser.BlockToken.Table _ ->
                        let collected, remaining = collectTableLines [] remainingBlocks

                        (createTable collected, remaining)

                    | BlockParser.BlockToken.InlineMetadata v ->

                        (p Style.none [ DOM.InlineContent.Text { Content = "" } ], remainingBlocks.Tail)
                    | BlockParser.BlockToken.Empty _ ->
                        (p Style.none [ DOM.InlineContent.Text { Content = "" } ], remainingBlocks.Tail)

                handler (append processedBlocks newBlock, newRemainingBlocks)

        handler ([], blocks)
        
    let parseMetadataKeyValue (line: string) =
        //let parse =
        let name = Regex.Match(line, """(?<=(<meta name="))([A-Za-z0-9\-:_]+)""")

        let content = Regex.Match(line, """(?<=(content="))([A-Za-z0-9\-:_\s\#\%]+)""")

        match name.Success, content.Success with
        | true, true -> Some(name.Value, content.Value)
        | _ -> None

type Parser(blocks: BlockParser.BlockToken list) =

    static member ExtractMetadata(lines: string list) =
        // Extract metadata.
        // TODO clean up.

        match lines.Length > 0 with
        | true ->
            let rec extract (acc, line: string, remaining: string list) =
                match line.StartsWith("<meta"), String.IsNullOrWhiteSpace(line) with
                | true, _ ->
                    match remaining |> List.tryHead with
                    | Some nl -> extract (acc @ [ line ], nl, remaining.Tail)
                    | None -> acc, remaining
                | _, true ->
                    match remaining |> List.tryHead with
                    | Some nl -> extract (acc, nl, remaining.Tail)
                    | None -> acc, remaining
                | false, false -> acc, line :: remaining

            let rawMetadata, remaining = extract ([], lines.Head, lines.Tail)

            let metadata =
                rawMetadata
                |> List.choose Processing.parseMetadataKeyValue
                |> Map.ofList

            metadata, remaining
        | false -> Map.empty, []

    static member ParseLines(lines) =

        let input = BlockParser.Input.Create(lines)

        let rec handler (state, i) =
            match BlockParser.tryParseBlock input i with
            | Some(token, next) -> handler (state @ [ token ], next)
            | None -> state

        Parser(BlockParser.parseBlocks input)

    static member ParseLinesAndMetadata(lines) =
        let rec extract (acc, remaining: string list) =
            match remaining |> List.tryHead with
            | Some l ->
                match l.StartsWith("<meta"), String.IsNullOrWhiteSpace(l) with
                | true, _ -> extract (acc @ [ l ], remaining.Tail)
                | _, true -> extract (acc, remaining.Tail)
                | false, false -> acc, remaining
            | None -> acc, remaining

        let rawMetadata, remaining = extract ([], lines)

        let input = BlockParser.Input.Create(remaining)

        let rec handler (state, i) =
            match BlockParser.tryParseBlock input i with
            | Some(token, next) -> handler (state @ [ token ], next)
            | None -> state

        let metadata =
            rawMetadata
            |> List.choose Processing.parseMetadataKeyValue
            |> Map.ofList

        Parser(BlockParser.parseBlocks input), metadata

    (*
    member parser.ExtractMetadata() =
        let rec extract (acc, line: string, remaining: string list) =
            match line.StartsWith("<meta"), String.IsNullOrWhiteSpace(line) with
            | true, _ ->
                match remaining |> List.tryHead with
                | Some nl -> extract (acc @ [ line ], nl, remaining.Tail)
                | None -> acc, remaining
            | _, true ->
                match remaining |> List.tryHead with
                | Some nl -> extract (acc, nl, remaining.Tail)
                | None -> acc, remaining
            | false, false -> acc, remaining

        ()
    *)
    //let (metadata, remaining) = extract ([], )


    //Parser(remaining)


    member parser.CreateBlockContent() = Processing.processBlocks blocks
