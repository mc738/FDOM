namespace FDOM.Rendering.Razor

open FDOM.Core.Common
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Rendering

[<AutoOpen>]
module private Utils =
    
    // TODO move to FUtils
    let private join separator (values: string seq) = System.String.Join(separator, values)

    /// Infix join.
    /// Example:
    /// let str = [ "1"; "2"; "3" ] +> ", "
    /// str = "1, 2, 3"
    let (+>) (items: string list) separator = join separator items
    
    let handleStyle style =
        match style with
        | DOM.Style.Ref r -> Some("style", r |> Seq.ofList |> join " ")  
        | DOM.Style.Custom map ->
            Some("class", map |> Map.toSeq |> Seq.map (fun (k, v) -> sprintf "%s: %s;" k v) |> join " ")
        | DOM.Style.Default -> None
            
    /// Create a RenderFragment
    let createFragment<'a> (value: 'a) (handler: 'a -> RenderTreeBuilder -> unit) = RenderFragment(handler value)
        
    let renderText (t: DOM.InlineText) = t.Content
    
    let renderSpan s step (b: RenderTreeBuilder) =
        b.AddContent(step, createFragment<DOM.InlineSpan> s (fun s b ->
                b.OpenElement(1, "span")
                let next =
                    match handleStyle s.Style with
                    | Some (tag, value) -> b.AddAttribute(2, tag, value); 3
                    | None -> 2
                b.AddContent(3, s.Content)
                b.CloseElement()))
            
    (*
    let renderSpans s step (b: RenderTreeBuilder) =
        s |> List.fold (fun step span ->
            b.AddContent(step, renderSpan span)
            step + 1 ) step
    *)
    
    let renderContentItem c step (b: RenderTreeBuilder) =
        match c with
        | DOM.InlineContent.Text t ->
            b.AddContent(step, (renderText t))
            step + 1
        | DOM.InlineContent.Span s ->
            renderSpan s step b
            step + 1
            
    let renderContent c step (b: RenderTreeBuilder) =
        c |> List.fold (fun step item -> (renderContentItem item step b)) step
        
    let renderStyle style (builder: RenderTreeBuilder) =
        match handleStyle style with
        | Some (tag, value) -> builder.AddAttribute(2, tag, value); 3
        | None -> 2
    
    let renderHeader h =
        createFragment<DOM.HeaderBlock> h (fun h b ->

            let tag =
                match h.Level with
                | DOM.HeaderLevel.H1 -> "h1"
                | DOM.HeaderLevel.H2  -> "h2"
                | DOM.HeaderLevel.H3  -> "h3"
                | DOM.HeaderLevel.H4  -> "h4"
                | DOM.HeaderLevel.H5  -> "h5"
                | DOM.HeaderLevel.H6  -> "h6"
            
            b.OpenElement(1, tag)
            
            // Handle the style and get the next index (if default style this is no op).
            let next = renderStyle h.Style b

            let _ = renderContent h.Content next b

            b.CloseElement())

    let renderParagraph p =
        createFragment<DOM.ParagraphBlock> p (fun p b ->
            b.OpenElement(1, "p")
            
          //  b.AddAttribute(2, attr, style)
            let next = renderStyle p.Style b

            let _ = renderContent p.Content next b

            b.CloseElement())
       
    let renderListItem li =
        createFragment<DOM.ListItem> li (fun li b ->
            b.OpenElement(1, "li")
            
            let next = renderStyle li.Style b

            let _ = renderContent li.Content next b
            
            b.CloseElement())
        
    let renderListItems l step (b: RenderTreeBuilder) =
        l |> List.fold (fun step item ->
            b.AddContent(step, renderListItem item)
            step + 1) step
        
    let renderList l =
        createFragment<DOM.ListBlock> l (fun l b ->
            
            let tag = match l.Ordered with | true -> "ol" | false -> "ul"
            
            b.OpenElement(1, tag)
            
          //  b.AddAttribute(2, attr, style)
            let next = renderStyle l.Style b

            let _ = renderListItems l.Items next b

            b.CloseElement())

    let renderBlock bl step (b: RenderTreeBuilder) =
        let rf =
            match bl with
            | DOM.BlockContent.Header h -> renderHeader h
            | DOM.BlockContent.Paragraph p -> renderParagraph p
            | DOM.BlockContent.List l -> renderList l
            | DOM.BlockContent.Image i -> failwith "Not implemented"

        b.AddContent(step, rf)
        step + 1
       
    let renderBlocks bls step (b: RenderTreeBuilder) =
        bls |> List.fold (fun step item -> (renderBlock item step b)) step
        
    let renderSection s =
        createFragment<DOM.Section> s (fun s b ->
            b.OpenElement(1, "section")
            
          //  b.AddAttribute(2, attr, style)
            let next = renderStyle s.Style b

            let _ = renderBlocks s.Content next b

            b.CloseElement())
    
    let renderSections s step (b: RenderTreeBuilder) =
        s |> List.fold (fun step item ->
            b.AddContent(step, renderSection item)
            step + 1) step    
        
        
    let renderDocument d =
        createFragment<DOM.Document> d (fun d b ->
            b.OpenElement(1, "article")
            
          //  b.AddAttribute(2, attr, style)
            let next = renderStyle d.Style b

            let _ = renderSections d.Sections next b

            b.CloseElement())


type Renderer =

    static member Render doc =
        renderDocument doc
