﻿[<RequireQualifiedAccess>]
module FDOM.Rendering.Html

open System
open System.Text.Encodings.Web
open FDOM.Core.Common
open Fluff.Core

type Layout = { Head: string; Foot: string }

let private join separator (values: string seq) = String.Join(separator, values)

/// Infix join.
/// Example:
/// let str = [ "1"; "2"; "3" ] +> ", "
/// str = "1, 2, 3"
let (+>) (items: string list) separator = join separator items

[<AutoOpen>]
module private Utils =
    let renderStyle style =

        match style with
        | DOM.Style.Ref r -> r |> Seq.ofList |> join " " |> sprintf " class=\"%s\"" // Note -> this takes care of the leading space.
        | DOM.Style.Custom map ->
            map
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> $"{k}: {v};")
            |> join " "
            |> sprintf " style=\"%s\"" // Note -> this takes care of the leading space.
        | DOM.Style.Combination(classes, styles) ->
            let c = classes |> join " "
            let s = styles |> Map.toList |> List.map (fun (k, v) -> $"{k}: {v}") |> join "; "
            $" class=\"{c}\" style=\"{s}\""
        | DOM.Style.Default -> ""

[<AutoOpen>]
module private Inline =
    let renderText (text: DOM.InlineText) =
        text.Content |> HtmlEncoder.Default.Encode

    let renderSpan (span: DOM.InlineSpan) =
        $"<span{renderStyle span.Style}>{span.Content}</span>"

    let renderLink (link: DOM.InlineLink) =
        $"""<a href="{link.Url}">{link.Content}</a>"""

    //let renderSpans (spans: DOM.InlineSpan list) = (spans |> List.map renderSpan) +> ""

    let renderInlineContent (content: DOM.InlineContent) =
        match content with
        | DOM.Text t -> renderText t
        | DOM.Span s -> renderSpan s
        | DOM.Link l -> renderLink l

    let renderInlineItems (items: DOM.InlineContent list) =
        (items |> List.map renderInlineContent) +> ""

    let renderInlineItemText (item: DOM.InlineContent) =
        match item with
        | DOM.Text t -> t.Content
        | DOM.Span s -> s.Content
        | DOM.Link l -> l.Content


    let slugify (name: string) =
        name
        |> Seq.fold
            (fun acc c ->
                match c with
                | _ when Char.IsLetterOrDigit c -> acc @ [ Char.ToLower c ]
                | _ when c = ' ' -> acc @ [ '_' ]
                | _ when c = '-' -> acc @ [ c ]
                | _ when c = '.' -> acc @ [ '-' ]
                | _ -> acc)
            []
        |> fun c -> String.Join("", c)

    let renderInlineItemsText (items: DOM.InlineContent list) =
        items |> List.map renderInlineItemText |> String.concat ""

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

        match header.Indexed with
        | true ->
            $"<{tag} id=\"{renderInlineItemsText header.Content |> slugify}\"{renderStyle header.Style}>{renderInlineItems header.Content}</{tag}>"
        | false -> $"<{tag}{renderStyle header.Style}>{renderInlineItems header.Content}</{tag}>"

    let renderParagraph (paragraph: DOM.ParagraphBlock) =
        $"<p{renderStyle paragraph.Style}>{renderInlineItems paragraph.Content}</p>"

    let renderCode (code: DOM.CodeBlock) =
        $"<pre{renderStyle code.Style}><code>{renderInlineItems code.Content}</code></pre>"

    let renderListItem (item: DOM.ListItem) =

        $"<li{renderStyle item.Style}>{renderInlineItems item.Content}</li>"

    let renderList (list: DOM.ListBlock) =

        let tag =
            match list.Ordered with
            | true -> "ol"
            | false -> "ul"

        let content = (list.Items |> List.map renderListItem) +> ""

        $"<{tag}{renderStyle list.Style}>{content}</{tag}>"

    let renderImage (img: DOM.ImageBlock) =
        let height =
            img.Height |> Option.map (fun h -> $" height=\"{h}\"") |> Option.defaultValue ""

        let width =
            img.Width |> Option.map (fun w -> $" width=\"{w}\"") |> Option.defaultValue ""

        $"""<img src="{img.Source}" alt="{img.AltText}" title="{img.Title}"{height}{width}{renderStyle img.Style}>"""

    let renderTable (table: DOM.TableBlock) =
        let header =
            table.Columns
            |> List.map (fun c ->
                // TODO handle alignment
                $"<th{renderStyle c.Style}>{renderInlineItems c.Content}</th>")
            |> String.concat ""

        let rows =
            table.Rows
            |> List.collect (fun r ->
                [ $"<tr{renderStyle r.Style}>"
                  yield!
                      r.Cells
                      |> List.map (fun c -> $"<td{renderStyle r.Style}>{renderInlineItems c.Content}</td>")
                  "</tr>" ])
            |> String.concat ""

        $"<table{renderStyle table.Style}><tr>{header}</tr><thead></thead><tbody>{rows}</tbody></table>"

    let renderBlock block =
        match block with
        | DOM.BlockContent.Header h -> renderHeader h
        | DOM.BlockContent.Paragraph p -> renderParagraph p
        | DOM.BlockContent.Code c -> renderCode c
        | DOM.BlockContent.List l -> renderList l
        | DOM.BlockContent.Image i -> renderImage i
        | DOM.BlockContent.Table t -> renderTable t

    let renderBlocks blocks = (blocks |> List.map renderBlock) +> ""

    let blockRewriter (fn: DOM.BlockContent -> DOM.BlockContent) (block: DOM.BlockContent) = fn block

[<AutoOpen>]
module private BoilerPlate =

    let renderHead title linksTags =
        $"""<!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><meta name="viewport" content="width=device-width, initial-scale=1.0"><title>{title}</title>{linksTags}</head><body>"""

    let renderFoot scriptTags = $"""{scriptTags}</body></html>"""

[<AutoOpen>]
module private Document =
    let renderSection (section: DOM.Section) =
        $"""<section{renderStyle section.Style}>{renderBlocks section.Content}</section>"""

    let renderBody layout content =
        [ layout.Head
          "<article>"
          (content |> List.map renderSection) +> ""
          "</article>"
          layout.Foot ]
        +> ""

    let renderStylesheetReference reference =
        $"""<link href="{reference}" rel="stylesheet">"""

    let renderScriptReference reference =
        $"""<script src="{reference}"></script>"""

let getIndexes (document: DOM.Document) =
    document.GetIndexes(renderInlineItemsText)

let renderInlineItems (items: DOM.InlineContent list) = Inline.renderInlineItems items

let render (layout: Layout) (stylesheets: string list) (scriptSources: string list) (document: DOM.Document) =

    let links = (stylesheets |> List.map renderStylesheetReference) +> ""

    let scripts = (scriptSources |> List.map renderScriptReference) +> ""

    [ renderHead document.Name links
      renderBody layout document.Sections
      renderFoot scripts ]
    +> ""

let renderFromParsedTemplate
    (template: Mustache.Token list)
    (values: Mustache.Data)
    (stylesheets: string list)
    (scriptSources: string list)
    (document: DOM.Document)
    =
    let links = (stylesheets |> List.map renderStylesheetReference) +> ""

    let scripts = (scriptSources |> List.map renderScriptReference) +> ""

    let renderArticle content =
        [ "<article>"; (content |> List.map renderSection) +> ""; "</article>" ] +> ""

    (*
    let indexes =
        Document.getIndexes document
        |> List.map
            (fun i ->
                [ "index_slug", i |> slugify |> Mustache.Value.Scalar
                  "index_title", i |> Mustache.Value.Scalar ]
                |> Map.ofList
                |> Mustache.Value.Object)
        |> Mustache.Value.Array
    *)


    let data =
        { values with
            Values = values.Values.Add("content", renderArticle document.Sections |> Mustache.Value.Scalar) }

    Mustache.replace data true template

let renderFromTemplate
    (template: string)
    (values: Mustache.Data)
    (stylesheets: string list)
    (scriptSources: string list)
    (document: DOM.Document)
    =
    renderFromParsedTemplate (Mustache.parse template) values stylesheets scriptSources document

let renderFromBlocks (blocks: DOM.BlockContent list) = renderBlocks blocks

let renderArticle (blocks: DOM.BlockContent list) =
    [ "<article>"; renderBlocks blocks; "</article>" ] +> ""

let renderBlocksWithTemplate (template: Mustache.Token list) (data: Mustache.Data) (blocks: DOM.BlockContent list) =
    //let render =
    //    [ "<article>"
    //      renderBlocks blocks
    //      "</article>" ]
    //    +> ""

    { data with
        Values = data.Values.Add("content", Mustache.Scalar <| renderArticle blocks) }
    |> fun d -> Mustache.replace d true template

let renderTitle (title: DOM.HeaderBlock) = renderHeader title

let renderDescription (description: DOM.ParagraphBlock) = renderParagraph description
