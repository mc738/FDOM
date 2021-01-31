namespace FDOM.Core.Common


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

    and ImageBlock = { Source: ImageSource; Style: Style }

    and ImageSource = Path of string

    and BlockContent =
        | Header of HeaderBlock
        | Paragraph of ParagraphBlock
        | List of ListBlock
        | Image of ImageBlock

    and InlineText = { Content: string }

    and InlineSpan = { Content: string; Style: Style }

    and InlineContent =
        | Text of InlineText
        | Spans of InlineSpan list


    //and Block =
    //    | Block of BlockContent
    //    | Inline of InlineContent list

    and Section =
        { Style: Style
          Title: HeaderBlock option
          Name: string
          Content: BlockContent list }

    and Document =
        { Style: Style
          Title: HeaderBlock option
          Name: string
          Sections: Section list }


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

    let createListItem style content: ListItem = { Style = style; Content = content }

    let createList ordered style items =
        BlockContent.List
            { Ordered = ordered
              Style = style
              Items = items }

    let createOrderedList style items = createList true style items

    let createUnorderedList style items = createList false style items

    let createImage style source =
        BlockContent.Image { Source = source; Style = style }


    let createText text = InlineContent.Text { Content = text }

    let createSpan style text = { Style = style; Content = text }

    let createSpans spans = InlineContent.Spans spans

    let createSection style title name content =
        { Style = style
          Title = title
          Name = name
          Content = content }

    let createDocument style title name sections =
        { Style = style
          Title = title
          Name = name
          Sections = sections }
