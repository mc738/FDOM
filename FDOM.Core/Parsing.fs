module FDOM.Core.Parsing

open System
open System.ComponentModel
open System.Runtime.Serialization
open System.Text
open FDOM.Core.Common
open FDOM.Core.Common
open FDOM.Core.Common
open FDOM.Core.Common
open FDOM.Core.Common
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
        | _ when
            Char.IsDigit(line.[0])
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
                  |> List.mapi
                      (fun i l ->
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
            match input.TryGetLine(curr + 1) with
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
            match input.TryGetLine(curr + 1) with
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

        let formatter =
            formatBlockText (Formatters.DefaultPreprocessors()) (Formatters.DefaultFormatters())

        let result =
            [ tryParseOrderedListItem formatter
              tryParseUnorderedListItem formatter
              tryParseCodeBlock formatter
              tryParseHeaderBlock
              tryParseParagraph formatter
              tryParseEmptyBlock ]
            |> List.fold
                (fun state h ->
                    match state with
                    | Error _ -> h input curr
                    | Ok r -> Ok r)
                (Error())

        match result with
        | Ok (token, i) -> Some(token, (i + 1))
        | Error _ -> None

    let parseBlocks input =
        let rec handler (state, i) =
            match tryParseBlock input i with
            | Some (token, next) -> handler (state @ [ token ], next)
            | None -> state
            
        handler ([], 0)

/// The inline parse takes block tokens and creates a DOM.
module InlineParser =

    let controlChars = [ '_'; '*'; '`' ]

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
            let s = str.[i..(i + pattern.Length - 1)]
            s = pattern
        | _ -> false

    /// Read from a index until a specified character.
    /// This returns the sub string and either index of the character of the next index alone if `inclusive is set to true`.
    let readUntilChar input character inclusive from =
        let rec handler (i) =
            match getChar input i with
            | Some c when c = character -> i
            | Some _ -> handler (i + 1)
            | None -> input.Length

        let endIndex = handler (from)

        (input.[from..(endIndex - 1)],
         if inclusive then
             endIndex + 1
         else
             endIndex)

    let readUntilCtrlChar input from =
        let rec handler (i) =
            match getChar input i with
            | Some c when isControlChar c -> i
            | Some _ -> handler (i + 1)
            | None -> input.Length

        let endIndex = handler (from)

        (input.[from..(endIndex - 1)], endIndex)

    /// Read from a index until a string.
    /// This returns the sub string and either index of the character of the next index alone if `inclusive is set to true`.
    let readUntilString input (pattern: string) inclusive from =
        let peek = compareLookAhead input pattern

        let rec handler (i) =
            match inBounds input (pattern.Length + i) with
            | true ->
                match peek i with
                | true -> i
                | false -> handler (i + 1)
            | false -> input.Length

        let endIndex = handler (from + pattern.Length - 1)

        (input.[(from + pattern.Length)..(endIndex - 1)],
         if inclusive then
             (endIndex + pattern.Length)
         else
             endIndex)

    //type

    let parseInlineContent (input: string) =

        // Cycle through input,
        // ` is completely delimited (

        let rec handler (state, i) =
            match getChar input i with
            | Some c ->
                //
                let (newState, next) =
                    match c with
                    | '*' ->
                        let ((sub, next), classes) =
                            match lookAhead input i 1, lookAhead input i 2 with
                            | Some (c1), Some (c2) when c1 = '*' && c2 = '*' ->
                                readUntilString input "***" true i, [ "b"; "i" ]
                            | Some (c1), _ when c1 = '*' -> readUntilString input "**" true i, [ "b" ]
                            | _ -> readUntilChar input '*' true (i + 1), [ "i" ]

                        let content =
                            DOM.InlineContent.Span
                                { Content = sub
                                  Style = DOM.Style.Ref classes }

                        (state @ [ content ], next)
                    | '`' ->
                        let (sub, next) = readUntilChar input '`' true (i + 1)
                        // Make this a span
                        let content =
                            DOM.InlineContent.Span
                                { Content = sub
                                  Style = DOM.Style.Ref [ "code" ] }

                        (state @ [ content ], next)
                    | _ ->
                        let (sub, next) = readUntilCtrlChar input i

                        let content = DOM.InlineContent.Text { Content = sub }

                        (state @ [ content ], next)

                handler (newState, next)
            | None -> state

        handler ([], 0)

module Processing =
   
    open FDOM.Core.Dsl
    open FDOM.Core.Dsl.General
    
    let getHeaderType (str : string) =
        
        let rec handler count (innerStr : char list) =
            match innerStr.[0] with
            | '#' -> handler (count + 1) innerStr.Tail
            | _ -> count
            
        let hType = handler 0 (str |> List.ofSeq)
        
        (hType, str.Substring(hType + 1))
    
    let createHeaderContent (value : string) =
        let (hType, content) = getHeaderType value
        match hType with
        | 1 -> h1 false Style.none (InlineParser.parseInlineContent content) 
        | 2 -> h2 false Style.none (InlineParser.parseInlineContent content) 
        | 3 -> h3 false Style.none (InlineParser.parseInlineContent content) 
        | 4 -> h4 false Style.none (InlineParser.parseInlineContent content) 
        | 5 -> h5 false Style.none (InlineParser.parseInlineContent content) 
        | 6 -> h6 false Style.none (InlineParser.parseInlineContent content) 
        | _ -> h6 false Style.none (InlineParser.parseInlineContent content) 
       
    let createParagraphContent (value : string) =
        p Style.none (InlineParser.parseInlineContent value)
        
    let createCodeBlock (value : string) =
        p (Style.references [ "codeblock" ]) (InlineParser.parseInlineContent value)
 
    let createListItem (value : string) =
        li Style.none (InlineParser.parseInlineContent value)
    
    let createOrderedListItems (values : string list) =
        ol Style.none (values |> List.map createListItem)

    let createUnorderedListItem (values : string list) =
        ul Style.none (values |> List.map createListItem)
      
    let rec collectOrderedListItems (collected : string list) (remaining : BlockParser.BlockToken list) =
        match remaining.Head with
        | BlockParser.BlockToken.OrderListItem v ->
            collectOrderedListItems (collected @ [ v ])  remaining.Tail
        | _ -> (collected, remaining)
    
    let rec collectUnorderedListItems (collected : string list) (remaining : BlockParser.BlockToken list) =
        match remaining.Head with
        | BlockParser.BlockToken.UnorderedListItem v ->
            collectUnorderedListItems (collected @ [ v ]) remaining.Tail
        | _ -> (collected, remaining)
       
    let private append (blocks : DOM.BlockContent list) block =
        blocks @ [ block ]
     
    /// Recursively process `BlockToken`s into `BlockContent`.
    let processBlocks (blocks : BlockParser.BlockToken list) =
        let rec handler (processedBlocks: DOM.BlockContent list, remainingBlocks : BlockParser.BlockToken list) =
            match remainingBlocks.IsEmpty with
            | true -> processedBlocks
            | false ->
                let (newBlock, newRemainingBlocks) =
                    match remainingBlocks.Head with
                    | BlockParser.BlockToken.Header h ->
                        (createHeaderContent h, remainingBlocks.Tail)
                    | BlockParser.BlockToken.Paragraph p ->
                        (createParagraphContent p, remainingBlocks.Tail)
                    | BlockParser.BlockToken.CodeBlock c ->
                        (createCodeBlock c, remainingBlocks.Tail)
                    | BlockParser.BlockToken.OrderListItem _ ->
                        let (collected, remaining) = collectOrderedListItems [] remainingBlocks
                        (createOrderedListItems collected, remaining)
                    | BlockParser.BlockToken.UnorderedListItem _ ->
                        let (collected, remaining) = collectUnorderedListItems [] remainingBlocks
                        (createUnorderedListItem collected, remaining)
                    | BlockParser.BlockToken.Empty _ -> (p Style.none [ DOM.InlineContent.Text { Content = "" }], remainingBlocks.Tail)
                handler(append processedBlocks newBlock, newRemainingBlocks)
                
        handler([], blocks)

type Parser(blocks : BlockParser.BlockToken list) =
    
    static member ParseLines(lines) =
        let input = BlockParser.Input.Create(lines)
        let rec handler (state, i) =
            match BlockParser.tryParseBlock input i with
            | Some (token, next) -> handler (state @ [ token ], next)
            | None -> state

        Parser(BlockParser.parseBlocks input)
    
    member parser.CreateBlockContent() = Processing.processBlocks blocks