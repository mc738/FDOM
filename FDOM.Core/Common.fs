namespace FDOM.Core.Common

open System
open System.Text.RegularExpressions

[<RequireQualifiedAccess>]
module DOM =

    type Style =
        | Ref of string list
        | Custom of Map<string, string>
        | Default

    // Blocks

    and HeaderBlock =
        { Style: Style
          Level: HeaderLevel
          Content: InlineContent list
          Indexed: bool }

        member h.GetRawText() = InlineContent.GetRawText h.Content

    and HeaderLevel =
        | H1
        | H2
        | H3
        | H4
        | H5
        | H6

    /// A paragraph block
    /// Can be
    and ParagraphBlock =
        { Style: Style
          Content: InlineContent list }

        member p.GetRawText() = InlineContent.GetRawText p.Content

    and CodeBlock =
        { Style: Style
          Content: InlineContent list }

        member c.GetRawText() = InlineContent.GetRawText c.Content

    /// A list block
    /// Can represent an ordered or unordered list.
    and ListBlock =
        { Ordered: bool
          Style: Style
          Items: ListItem list }

    /// A list item.
    /// List items content as `sub sections` and as such can contain blocks.
    and ListItem =
        { Style: Style
          Content: InlineContent list }

    and ImageBlock =
        { Source: string
          Title: string
          AltText: string
          Height: string option
          Width: string option
          Style: Style }

    and BlockContent =
        | Header of HeaderBlock
        | Paragraph of ParagraphBlock
        | Code of CodeBlock
        | List of ListBlock
        | Image of ImageBlock

        member bc.GetRawText() =
            match bc with
            | Header h -> h.GetRawText()
            | Paragraph p -> p.GetRawText()
            | Code c -> c.GetRawText()
            | List _ -> ""
            | Image i -> i.Title


    and InlineText = { Content: string }

    and InlineSpan = { Content: string; Style: Style }

    and InlineLink =
        { Content: string
          Url: string
          Style: Style }

    and InlineContent =
        | Text of InlineText
        | Span of InlineSpan
        | Link of InlineLink

        static member GetRawText(content: InlineContent list) =
            content
            |> List.map (fun c ->
                match c with
                | InlineContent.Text t -> t.Content
                | InlineContent.Span s -> s.Content
                | InlineContent.Link l -> l.Content)
            |> String.concat ""

    and Section =
        { Style: Style
          Title: HeaderBlock option
          Name: string
          Content: BlockContent list }

        /// Get indexed headers from a section and pass the into a handler function to generate indexes.
        member section.GetIndexes<'I>(indexHandler: InlineContent list -> 'I) =
            section.Content
            |> List.choose (fun c ->
                match c with
                | Header h ->
                    match h.Indexed with
                    | true -> Some <| indexHandler h.Content
                    | false -> None
                | _ -> None)

    and Resource =
        { Name: string
          Path: string
          VirtualPath: string
          Type: string }

    and Document =
        { Style: Style
          Title: HeaderBlock option
          Name: string
          Sections: Section list
          Resources: Resource list }

        member doc.SnakeCaseName =
            doc.Name.ToLower().Replace(' ', '_')

        /// Get indexed headers from a document and pass the into a handler function to generate indexes.
        member doc.GetIndexes(indexHandler: InlineContent list -> string) =
            doc.Sections
            |> List.collect (fun s -> s.GetIndexes indexHandler)
            
        member doc.GetTitleText() =
            doc.Title |> Option.map (fun t -> t.GetRawText()) |> Option.defaultValue "[unnamed document]"

    type RenderedDocument = { Path: string; VirtualPath: string }

    /// A helper to create a header block.
    /// The function is set up to allow partial application.
    /// For example:
    /// let createHeader1 indexed style content = createHeader HeaderLevel.H1 indexed style content
    let createHeader headerLevel indexed style content =
        BlockContent.Header
            { Style = style
              Level = headerLevel
              Content = content
              Indexed = indexed }

    let createH1 indexed style content =
        createHeader HeaderLevel.H1 indexed style content

    let createH2 indexed style content =
        createHeader HeaderLevel.H2 indexed style content

    let createH3 indexed style content =
        createHeader HeaderLevel.H3 indexed style content

    let createH4 indexed style content =
        createHeader HeaderLevel.H4 indexed style content

    let createH5 indexed style content =
        createHeader HeaderLevel.H5 indexed style content

    let createH6 indexed style content =
        createHeader HeaderLevel.H6 indexed style content

    let createParagraph style content =
        BlockContent.Paragraph { Style = style; Content = content }

    let createCode style content =
        BlockContent.Code { Style = style; Content = content }

    let createListItem style content : ListItem = { Style = style; Content = content }

    let createList ordered style items =
        BlockContent.List
            { Ordered = ordered
              Style = style
              Items = items }

    let createOrderedList style items = createList true style items

    let createUnorderedList style items = createList false style items

    let createImage style source title altText height width =
        { Source = source
          Title = title
          AltText = altText
          Height = height
          Width = width
          Style = style }
        |> BlockContent.Image

    let createText text = InlineContent.Text { Content = text }

    let createSpan style text =
        InlineContent.Span { Style = style; Content = text }


    let createLink style text url =
        InlineContent.Link
            { Style = style
              Content = text
              Url = url }

    let createSection style title name content =
        { Style = style
          Title = title
          Name = name
          Content = content }

    let createDocument style title name sections resources =
        { Style = style
          Title = title
          Name = name
          Sections = sections
          Resources = resources }

module Formatting =

    /// A regex replace formatter.
    /// This will take a pattern and replace it with a replacement string.
    type RegexReplaceFormatter =
        { Pattern: string
          Replacement: string }

        static member CreateFormatters(data: (string * string) list) =
            data
            |> List.map (fun (p, r) -> Formatter.RegexReplace { Pattern = p; Replacement = r })

        member formatter.Run(input) =
            Regex.Replace(input, formatter.Pattern, formatter.Replacement)

    /// A string replace formatter.
    /// This will take a string and replace it with a replacement string.
    and StringReplaceFormatter =
        { Pattern: string
          Replacement: string }

        static member CreateFormatters(data: (string * string) list) =
            data
            |> List.map (fun (p, r) -> Formatter.StringReplace { Pattern = p; Replacement = r })

        member formatter.Run(input: string) =
            input.Replace(formatter.Pattern, formatter.Replacement)

    /// A formatter that can be used to transform text.
    and Formatter =
        | RegexReplace of RegexReplaceFormatter
        | StringReplace of StringReplaceFormatter
        | Trim
        | AddSpaceToLineEnd

    let private formatterHandler formatter input =
        match formatter with
        | RegexReplace f -> f.Run(input)
        | StringReplace f -> f.Run(input)
        | Trim -> input.Trim()
        | AddSpaceToLineEnd ->
            if String.IsNullOrEmpty input |> not
               && input.[input.Length - 1] <> ' ' then
                $"{input} "
            else
                input

    type Formatters =
        { Items: Formatter list }
        static member Create(formatters) = { Items = formatters }


        static member DefaultPreprocessors() =
            let regexMacros =
                RegexReplaceFormatter.CreateFormatters([ "\%NOW\%", DateTime.Now.ToString("hh:mm dd MMM yy") ])

            Formatters.Create(
                List.concat [ regexMacros
                              [ Trim; AddSpaceToLineEnd ] ]
            )

        static member DefaultFormatters() =
            // Most of these might not been needed anymore with `AddSpaceToLineEnd`.
            let regexReplacements =
                RegexReplaceFormatter.CreateFormatters(
                    [ "\,(?=[A-Za-z0-9])", ", " // Add space after , if missing.
                      //"\.(?=[A-Za-z0-9])", ". " // Add space after . if missing.
                      "\!(?=[A-Za-z0-9])", "! " // Add space after ! if missing.
                      "\?(?=[A-Za-z0-9])", "? " // Add space after ? if missing.
                      "^(\* )", "" // Strip leading `* `
                      "^([0-9]\. )|^([0-9][0-9]\. )", "" ] // Strip leading `1. ` or `99. `
                )

            // One final trim to remove any extra spaces. Could be more efficient but it should work.
            Formatters.Create(
                List.concat [ regexReplacements
                              [ Trim ] ]
            )

        member formatters.Run(input) =
            formatters.Items
            |> List.fold (fun state f -> formatterHandler f state) input
