[<RequireQualifiedAccess>]
module FDOM.Rendering.Pdf

open FDOM.Core.Common
open FreDF.Core
open MigraDocCore.DocumentObjectModel


type Layout = { Head: string; Foot: string }

type PdfRendererSettings =
    { StylePath: string
      DefaultUnderLineType: Style.Underline
      DefaultSectionHeader: Structure.HeaderFooter option
      DefaultSectionFooter: Structure.HeaderFooter option
      DefaultSectionPageSetup: Structure.PageSetup option
      H1Class: string
      H2Class: string
      H3Class: string
      H4Class: string
      H5Class: string
      H6Class: string }

    static member Default() =
        { StylePath = ""
          DefaultUnderLineType = Style.Underline.Single
          DefaultSectionPageSetup = None 
          H1Class = "Header1"
          H2Class = "Header2"
          H3Class = "Header3"
          H4Class = "Header4"
          H5Class = "Header5"
          H6Class = "Header6" }


// TODO move to FUtils
let private join separator (values: string seq) = System.String.Join(separator, values)

/// Infix join.
/// Example:
/// let str = [ "1"; "2"; "3" ] +> ", "
/// str = "1, 2, 3"
let (+>) (items: string list) separator = join separator items

[<AutoOpen>]
module private Inline =
    let renderText (text: DOM.InlineText) =
        Elements.ParagraphElement.Text({ Content = text.Content }: Elements.Text)

    let renderSpan (span: DOM.InlineSpan) =
        // For not these will assume there is no predefined style (due to the various combinations
        let classes, styles =
            match span.Style with
            | DOM.Ref l -> Some l, None
            | DOM.Custom stringMap -> None, Some stringMap
            | DOM.Combination(classes, styles) -> Some classes, Some styles
            | DOM.Default -> None, None

        let isCode =
            classes
            |> Option.map (List.contains PredefinedStyleRefs.code)
            |> Option.defaultValue false

        Elements.ParagraphElement.FormattedText(
            { Bold = classes |> Option.map (fun c -> c |> List.contains PredefinedStyleRefs.bold)
              Color = styles |> Option.bind (Map.tryFind "color") |> Option.map deserializeColor
              Italic = classes |> Option.map (fun c -> c |> List.contains PredefinedStyleRefs.italics)
              Size = None
              Subscript = None
              Superscript = None
              Underline =
                classes
                |> Option.bind (fun c ->
                    // TODO make this configurable
                    if c |> List.contains "ul" then
                        Some Style.Underline.Single
                    else
                        None)
              Font =
                styles
                |> Option.bind (fun s -> s.TryFind "font-family")
                |> Option.map (fun f ->
                    ({ Bold = None
                       Color = None
                       Italic = None
                       Name = Some f
                       Size = None
                       Subscript = None
                       Superscript = None
                       Underline = None }
                    : Style.Font))
                |> Option.orElseWith (fun _ ->
                    match isCode with
                    | true ->
                        ({ Bold = None
                           Color = None
                           Italic = None
                           Name = Some "monospace" // TODO make this configurable
                           Size = None
                           Subscript = None
                           Superscript = None
                           Underline = None }
                        : Style.Font)
                        |> Some
                    | false -> None)
              Style = failwith "todo"
              Elements = [ Elements.ParagraphElement.Text({ Content = span.Content }: Elements.Text) ] }
            : Elements.FormattedText
        )
    //span.Content // TODO add spans to FreDF

    let renderLink (span: DOM.InlineLink) =

        ()

    //let renderSpans (spans: DOM.InlineSpan list) = (spans |> List.map renderSpan) +> ""

    let renderInlineContent (content: DOM.InlineContent) =
        match content with
        | DOM.Text t -> renderText t
        | DOM.Span s -> renderSpan s
        | DOM.Link inlineLink -> failwith "todo"

    let renderInlineItems (items: DOM.InlineContent list) = (items |> List.map renderInlineContent) //+> ""


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

    let renderParagraph (paragraph: DOM.ParagraphBlock) =
        ({ Elements = renderInlineItems paragraph.Content
           Format = None
           Style = None }
        : Elements.Paragraph)
        |> Elements.DocumentElement.Paragraph

    let renderListItem (item: DOM.ListItem) =
        Elements.p (renderInlineItems item.Content) // TODO handle list items

    let renderList (list: DOM.ListBlock) =

        let tag =
            match list.Ordered with
            | true -> "ol"
            | false -> "ul"

        (list.Items |> List.map renderListItem)

    let renderImage (image: DOM.ImageBlock) =
        ({ Source = image.Source
           Height = failwith "todo"
           Width = failwith "todo"
           Left = failwith "todo"
           Top = failwith "todo"
           Resolution = failwith "todo"
           ScaleHeight = failwith "todo"
           ScaleWidth = failwith "todo"
           LockAspectRatio = failwith "todo" }
        : Elements.Image)
        |> Elements.DocumentElement.Image

    let renderBlock block =
        match block with
        | DOM.BlockContent.Header h -> [ renderHeader h ]
        | DOM.BlockContent.Paragraph p -> [ renderParagraph p ]
        | DOM.BlockContent.List l -> renderList l
        | DOM.BlockContent.Image i -> [ renderImage i ]
        | DOM.Code codeBlock ->
            //Elements.

            failwith "todo"
        | DOM.Table tableBlock -> failwith "todo"

    let renderBlocks blocks =
        blocks |> List.map renderBlock |> List.concat


    let elementBuilder (elements: Elements.DocumentElement list) (section: MigraDocCore.DocumentObjectModel.Section) =
        ({ PageSetup = None
           Headers = failwith "todo"
           Footers = failwith "todo"
           Elements = elements }
        : Structure.Section)
            .ToDocObj()


[<AutoOpen>]
module private Document =

    let renderSection (settings: PdfRendererSettings) (section: DOM.Section) =
        ({ PageSetup = settings.DefaultSectionPageSetup
           Headers = failwith "todo"
           Footers = failwith "todo"
           Elements = renderBlocks section.Content }
        : Structure.Section)
            .ToDocObj()


    let renderBody content = content |> List.map renderSection

let render (savePath: string) (stylePath: string) (document: DOM.Document) =
    Pdf.init stylePath
    |> Pdf.build (renderBody document.Sections)
    |> Pdf.render savePath
