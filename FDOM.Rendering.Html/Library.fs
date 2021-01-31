[<RequireQualifiedAccess>]
module FDOM.Rendering.Html

open FDOM.Core.Common

type Layout = { Head: string; Foot: string }

// TODO move to FUtils
let private join separator (values: string seq) = System.String.Join(separator, values)

/// Infix join.
/// Example:
/// let str = [ "1"; "2"; "3" ] +> ", "
/// str = "1, 2, 3"
let (+>) (items: string list) separator = join separator items

[<AutoOpen>]
module private Utils =
    let renderStyle style =

        match style with
        | DOM.Style.Ref r ->
            r
            |> Seq.ofList
            |> join " "
            |> sprintf " class=\"%s\"" // Note -> this takes care of the leading space.
        | DOM.Style.Custom map ->
            map
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> sprintf "%s: %s;" k v)
            |> join " "
            |> sprintf " style=\"%s\"" // Note -> this takes care of the leading space.
        | DOM.Style.Default -> ""

[<AutoOpen>]
module private Inline =
    let renderText (text: DOM.InlineText) = text.Content

    let renderSpan (span: DOM.InlineSpan) =
        sprintf "<span%s>%s</span>" (renderStyle span.Style) span.Content

    let renderSpans (spans: DOM.InlineSpan list) = (spans |> List.map renderSpan) +> ""

    let renderInlineContent (content: DOM.InlineContent) =
        match content with
        | DOM.Text t -> renderText t
        | DOM.Spans s -> renderSpans s

    let renderInlineItems (items: DOM.InlineContent list) =
        (items |> List.map renderInlineContent) +> ""

[<AutoOpen>]
module private Blocks =
    let renderHeader (header: DOM.HeaderBlock) =
        let tag =
            match header.Level with
            | DOM.HeaderLevel.H1 -> "h1"
            | DOM.HeaderLevel.H2 -> "h2"
            | DOM.HeaderLevel.H3 -> "h3"
            | DOM.HeaderLevel.H4 -> "h4"
            | DOM.HeaderLevel.H5 -> "h5"
            | DOM.HeaderLevel.H6 -> "h6"

        sprintf "<%s%s>%s</%s>" tag (renderStyle header.Style) (renderInlineItems header.Content) tag

    let renderParagraph (paragraph: DOM.ParagraphBlock) =
        sprintf "<p%s>%s</p>" (renderStyle paragraph.Style) (renderInlineItems paragraph.Content)

    let renderListItem (item: DOM.ListItem) =

        sprintf "<li%s>%s</li>" (renderStyle item.Style) (renderInlineItems item.Content)

    let renderList (list: DOM.ListBlock) =

        let tag =
            match list.Ordered with
            | true -> "ol"
            | false -> "ul"

        let content =
            (list.Items |> List.map renderListItem) +> ""

        sprintf "<%s%s>%s</%s>" tag (renderStyle list.Style) content tag

    let renderImage = "Not implemented" // TODO Implement this!

    let renderBlock block =
        match block with
        | DOM.BlockContent.Header h -> renderHeader h
        | DOM.BlockContent.Paragraph p -> renderParagraph p
        | DOM.BlockContent.List l -> renderList l
        | DOM.BlockContent.Image i -> renderImage

    let renderBlocks blocks = (blocks |> List.map renderBlock) +> ""

[<AutoOpen>]
module private BoilerPlate =

    let renderHead title linksTags =
        sprintf
            """<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>%s</title>%s</head><body>"""
            title
            linksTags

    let renderFoot scriptTags =
        sprintf """%s</body></html>""" scriptTags

[<AutoOpen>]
module private Document =
    let renderSection (section: DOM.Section) =
        sprintf """<section%s>%s</section>""" (renderStyle section.Style) (renderBlocks section.Content)

    let renderBody layout content =
        [ layout.Head
          "<article>"
          (content |> List.map renderSection) +> ""
          "</article>"
          layout.Foot ]
        +> ""



    let renderStylesheetReference reference =
        sprintf """<link href="%s" rel="stylesheet">""" reference

    let renderScriptReference reference =
        sprintf """<script src="%s"></script>""" reference

let render (layout: Layout) (stylesheets: string list) (scriptSources: string list) (document: DOM.Document) =

    let links =
        (stylesheets |> List.map renderStylesheetReference)
        +> ""

    let scripts =
        (scriptSources |> List.map renderScriptReference)
        +> ""

    [ renderHead document.Name links
      renderBody layout document.Sections
      renderFoot scripts ]
    +> ""
