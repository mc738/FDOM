[<RequireQualifiedAccess>]
module FDOM.Rendering.Pdf

open FDOM.Core.Common
open FreDF.Core


type Layout = { Head: string; Foot: string }

// TODO move to FUtils
let private join separator (values: string seq) = System.String.Join(separator, values)

/// Infix join.
/// Example:
/// let str = [ "1"; "2"; "3" ] +> ", "
/// str = "1, 2, 3"
let (+>) (items: string list) separator = join separator items

[<AutoOpen>]
module private Inline =
    let renderText (text: DOM.InlineText) = text.Content

    let renderSpan (span: DOM.InlineSpan) = span.Content // TODO add spans to FreDF
    
    //let renderSpans (spans: DOM.InlineSpan list) = (spans |> List.map renderSpan) +> ""

    let renderInlineContent (content: DOM.InlineContent) =
        match content with
        | DOM.Text t -> renderText t
        | DOM.Span s -> renderSpan s

    let renderInlineItems (items: DOM.InlineContent list) =
        (items |> List.map renderInlineContent) +> ""


[<AutoOpen>]
module private Blocks =
    let renderHeader (header: DOM.HeaderBlock) =
        let tag =
            match header.Level with
            | DOM.HeaderLevel.H1 -> Elements.h1
            | DOM.HeaderLevel.H2 -> Elements.h2
            | DOM.HeaderLevel.H3 -> Elements.h3
            | DOM.HeaderLevel.H4 -> Elements.h4
            | DOM.HeaderLevel.H5 -> Elements.h5
            | DOM.HeaderLevel.H6 -> Elements.h6

        tag (renderInlineItems header.Content)

    let renderParagraph (paragraph: DOM.ParagraphBlock) = Elements.p (renderInlineItems paragraph.Content)
    
    let renderListItem (item: DOM.ListItem) = Elements.p (renderInlineItems item.Content) // TODO handle list items


    let renderList (list: DOM.ListBlock) =

        let tag =
            match list.Ordered with
            | true -> "ol"
            | false -> "ul"

        (list.Items |> List.map renderListItem)

    let renderImage (image: DOM.ImageBlock) = Elements.img "" "" true //  "Not implemented" // TODO Implement this!

    let renderBlock block =
        match block with
        | DOM.BlockContent.Header h -> [ renderHeader h ]
        | DOM.BlockContent.Paragraph p -> [ renderParagraph p ]
        | DOM.BlockContent.List l -> renderList l
        | DOM.BlockContent.Image i -> [ renderImage i ]

    let renderBlocks blocks = blocks |> List.map renderBlock |> List.concat
    
[<AutoOpen>]
module private Document =
    let renderSection (section: DOM.Section) =
        {
            Portrait = true
            Elements = (renderBlocks section.Content)
        }
        
    let renderBody content = content |> List.map renderSection

let render (savePath: string) (stylePath: string) (document: DOM.Document) =
    Pdf.init stylePath
    |> Pdf.build (renderBody document.Sections)
    |> Pdf.render savePath