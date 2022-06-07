module FDOM.Core.Serialization

open System.IO
open System.Text.Json
open FDOM.Core
open FDOM.Core.Common


[<AutoOpen>]
/// A collection of helper functions for `System.Text.Json`
module JsonHelpers =

    let tryGetProperty (name: string) (element: JsonElement) =
        match element.TryGetProperty name with
        | true, prop -> Some prop
        | false, _ -> None

    let tryGetByte (element: JsonElement) =
        match element.TryGetByte() with
        | true, b -> Some b
        | false, _ -> None

    let tryGetDecimal (element: JsonElement) =
        match element.TryGetDecimal() with
        | true, d -> Some d
        | false, _ -> None

    let tryGetDouble (element: JsonElement) =
        match element.TryGetDouble() with
        | true, d -> Some d
        | false, _ -> None

    let tryGetGuid (element: JsonElement) =
        match element.TryGetGuid() with
        | true, g -> Some g
        | false, _ -> None

    let tryGetInt16 (element: JsonElement) =
        match element.TryGetInt16() with
        | true, i -> Some i
        | false, _ -> None

    let tryGetInt (element: JsonElement) =
        match element.TryGetInt32() with
        | true, i -> Some i
        | false, _ -> None

    let tryGetInt64 (element: JsonElement) =
        match element.TryGetInt64() with
        | true, i -> Some i
        | false, _ -> None

    let tryGetSingle (element: JsonElement) =
        match element.TryGetSingle() with
        | true, s -> Some s
        | false, _ -> None

    let tryGetDateTime (element: JsonElement) =
        match element.TryGetDateTime() with
        | true, d -> Some d
        | false, _ -> None

    let tryGetSByte (element: JsonElement) =
        match element.TryGetSByte() with
        | true, s -> Some s
        | false, _ -> None

    let tryGetUInt16 (element: JsonElement) =
        match element.TryGetUInt16() with
        | true, u -> Some u
        | false, _ -> None

    let tryGetUInt (element: JsonElement) =
        match element.TryGetUInt32() with
        | true, u -> Some u
        | false, _ -> None

    let tryGetUInt64 (element: JsonElement) =
        match element.TryGetUInt64() with
        | true, u -> Some u
        | false, _ -> None

    let tryGetBytesFromBase64 (element: JsonElement) =
        match element.TryGetBytesFromBase64() with
        | true, b -> Some b
        | false, _ -> None

    let tryGetStringProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> Some(p.GetString())
        | None -> None

    let tryGetBoolProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> Some(p.GetBoolean())
        | None -> None

    let tryGetByteProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetByte p
        | None -> None

    let tryGetDecimalProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetDecimal p
        | None -> None

    let tryGetDoubleProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetDouble p
        | None -> None

    let tryGetGuidProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetGuid p
        | None -> None

    let tryGetInt16Property (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetInt16 p
        | None -> None

    let tryGetIntProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetInt p
        | None -> None

    let tryGetInt64Property (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetInt64 p
        | None -> None

    let tryGetSingleProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetSingle p
        | None -> None

    let tryGetDateTimeProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetDateTime p
        | None -> None

    let tryGetSByteProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetSByte p
        | None -> None

    let tryGetUInt16Property (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetUInt16 p
        | None -> None

    let tryGetUIntProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetUInt p
        | None -> None

    let tryGetUInt64Property (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetUInt64 p
        | None -> None

    let tryGetBytesFromBase64Property (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> tryGetBytesFromBase64 p
        | None -> None


    let tryGetElementsProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> Some(p.EnumerateObject() |> List.ofSeq)
        | None -> None

    let tryGetArrayProperty (name: string) (element: JsonElement) =
        match tryGetProperty name element with
        | Some p -> Some(p.EnumerateArray() |> List.ofSeq)
        | None -> None

    //let writeInt (writer : Utf8JsonWriter) (name : string) (value : byte) = writer.WriteNumber(name, byte)

    let propertiesToStringMap (properties: JsonProperty list) =
        properties
        |> List.map (fun el -> (el.Name, el.Value.GetString()))
        |> Map.ofList


    let writeString (writer: Utf8JsonWriter) (name: string) (value: string) = writer.WriteString(name, value)

[<AutoOpen>]
module private Internal =

    [<AutoOpen>]
    module ErrorMessages =

        let missingProperty name propertyName =
            $"`{name}` style types require a `{propertyName}` property."

        let missingTypeSpecificProperty typeName name propertyName =
            $"`{name}` {typeName} types require a `{propertyName}` property."

    let tryGetName = tryGetStringProperty "name"

    let tryGetType = tryGetStringProperty "type"

    let tryGetStyle = tryGetProperty "style"

    let tryGetContent = tryGetArrayProperty "content"

    let writePropertyObject (handler: Utf8JsonWriter -> unit) (name: string) (writer: Utf8JsonWriter) =
        writer.WriteStartObject(name)
        handler writer
        writer.WriteEndObject()

    let writeArray (handler: Utf8JsonWriter -> unit) (name: string) (writer: Utf8JsonWriter) =
        writer.WriteStartArray(name)
        handler writer
        writer.WriteEndArray()

    let writeObject (handler: Utf8JsonWriter -> unit) (writer: Utf8JsonWriter) =
        writer.WriteStartObject()
        handler writer
        writer.WriteEndObject()


[<AutoOpen>]
/// Extensions for core `FDOM` types to handle serialization.
/// These are used to serialize documents to json using `System.Text.Json`
module Extensions =    
    
    type DOM.Style with

        static member FromJson(element: JsonElement) =

            let typeMessage = missingTypeSpecificProperty "style"

            let styleType = tryGetStringProperty "type" element

            match styleType with
            | Some t ->
                match t with
                | "custom" ->
                    match tryGetElementsProperty "data" element with
                    | Some p -> Ok(DOM.Style.Custom(propertiesToStringMap p))
                    | None -> Error(typeMessage "custom" "data")
                | "ref" ->
                    match tryGetArrayProperty "selectors" element with
                    | Some a -> Ok(DOM.Style.Ref(a |> List.map (fun el -> el.GetString())))
                    | None -> Error "`ref` style types require a `selector` array."
                | "default" -> Ok DOM.Style.Default
                | _ -> Error $"Unknown style type '{t}'"
            | None -> Error "Missing `type` property in style."

        static member FromJsonWithDefault(element: JsonElement) =
            match DOM.Style.FromJson element with
            | Ok s -> s
            | Error _ -> DOM.Style.Default

        member style.WriteToJson(writer: Utf8JsonWriter) =
            match style with
            | DOM.Style.Custom map ->
                writer.WriteString("type", "custom")
                writer.WriteStartObject("data")

                map
                |> Map.map (fun k v -> writer.WriteString(k, v))
                |> ignore

                writer.WriteEndArray()
            | DOM.Style.Ref classes ->
                writer.WriteString("type", "ref")
                writer.WriteStartArray("selectors")

                classes
                |> List.map writer.WriteStringValue
                |> ignore

                writer.WriteEndArray()
            | DOM.Style.Default -> writer.WriteString("type", "default")

    type DOM.InlineText with

        static member FromJson(element: JsonElement) =
            match tryGetStringProperty "value" element with
            | Some s -> Ok({ Content = s }: DOM.InlineText)
            | None -> Error "Missing `value` property"

        member text.WriteToJson(writer: Utf8JsonWriter) =
            writer.WriteString("type", "text")
            writer.WriteString("value", text.Content)

    type DOM.InlineSpan with

        static member FromJson(element: JsonElement) =
            match tryGetStringProperty "value" element, tryGetStyle element with
            | Some s, Some se ->
                Ok(
                    { Content = s
                      Style = DOM.Style.FromJsonWithDefault se }: DOM.InlineSpan
                )
            | Some s, None ->
                Ok(
                    { Content = s
                      Style = DOM.Style.Default }: DOM.InlineSpan
                )
            | None, _ -> Error "Missing `value` property."

        member span.WriteToJson(writer: Utf8JsonWriter) =
            writer.WriteString("type", "span")
            writePropertyObject span.Style.WriteToJson "style" writer
            writer.WriteString("value", span.Content)

    type DOM.InlineContent with

        static member FromJson(element: JsonElement) =
            match tryGetType element with
            | Some ct when ct = "text" ->
                match DOM.InlineText.FromJson element with
                | Ok t -> Ok(DOM.InlineContent.Text t)
                | Error e -> Error $"Could not create text element. Error: \"{e}\""
            | Some ct when ct = "span" ->
                match DOM.InlineSpan.FromJson element with
                | Ok s -> Ok(DOM.InlineContent.Span s)
                | Error e -> Error $"Could not create span element. Error: \"{e}\""
            | Some ct -> Error $"Unknown inline content type '{ct}'."
            | None -> Error "In"

        member content.WriteToJson(writer: Utf8JsonWriter) =
            match content with
            | DOM.InlineContent.Text t -> t.WriteToJson writer
            | DOM.InlineContent.Span s -> s.WriteToJson writer

    type DOM.HeaderBlock with

        static member FromJson(element: JsonElement) =

            let headerLevel = tryGetIntProperty "level" element

            let indexed =
                tryGetBoolProperty "indexed" element
                |> Option.defaultValue false

            let styleElement = tryGetStyle element
            let contentElement = tryGetContent element

            match (headerLevel, styleElement, contentElement) with
            | Some hl, Some se, Some ce ->
                let l =
                    match hl with
                    | 1 -> DOM.HeaderLevel.H1
                    | 2 -> DOM.HeaderLevel.H2
                    | 3 -> DOM.HeaderLevel.H3
                    | 4 -> DOM.HeaderLevel.H4
                    | 5 -> DOM.HeaderLevel.H5
                    | 6 -> DOM.HeaderLevel.H6
                    | _ -> DOM.HeaderLevel.H6

                let style =
                    match DOM.Style.FromJson se with
                    | Ok s -> s
                    | Error e -> DOM.Style.Default

                let (content, errors) =
                    ce
                    |> List.map DOM.InlineContent.FromJson
                    |> Utils.collectResults

                Ok(
                    { Level = l
                      Indexed = indexed
                      Style = style
                      Content = content }: DOM.HeaderBlock
                )
            | None, _, _ -> Error "Missing header level."
            | _, None, _ -> Error "Missing style element."
            | _, _, None -> Error "Missing content element."

        member header.WriteToJson(writer: Utf8JsonWriter) =
            let level =
                match header.Level with
                | DOM.HeaderLevel.H1 -> 1
                | DOM.HeaderLevel.H2 -> 2
                | DOM.HeaderLevel.H3 -> 3
                | DOM.HeaderLevel.H4 -> 4
                | DOM.HeaderLevel.H5 -> 5
                | DOM.HeaderLevel.H6 -> 6

            writer.WriteString("type", "h")
            writer.WriteNumber("level", level)
            writer.WriteBoolean("indexed", header.Indexed)
            writePropertyObject header.Style.WriteToJson "style" writer

            writeArray
                (fun w ->
                    header.Content
                    |> List.map (fun c -> writeObject c.WriteToJson w)
                    |> ignore)
                "content"
                writer

    type DOM.ParagraphBlock with

        static member FromJson(element: JsonElement) =
            match tryGetContent element, tryGetStyle element with
            | Some ce, Some se ->
                let (content, errors) =
                    ce
                    |> List.map DOM.InlineContent.FromJson
                    |> Utils.collectResults

                Ok(
                    { Style = DOM.Style.FromJsonWithDefault se
                      Content = content }: DOM.ParagraphBlock
                )
            | None, _ -> Error "Missing content element."
            | _, None -> Error "Missing style element."

        member paragraph.WriteToJson(writer: Utf8JsonWriter) =
            writer.WriteString("type", "p")
            writePropertyObject paragraph.Style.WriteToJson "style" writer

            writeArray
                (fun w ->
                    paragraph.Content
                    |> List.map (fun c -> writeObject c.WriteToJson w)
                    |> ignore)
                "content"
                writer
                
    type DOM.CodeBlock with

        static member FromJson(element: JsonElement) =
            match tryGetContent element, tryGetStyle element with
            | Some ce, Some se ->
                let (content, errors) =
                    ce
                    |> List.map DOM.InlineContent.FromJson
                    |> Utils.collectResults

                Ok(
                    { Style = DOM.Style.FromJsonWithDefault se
                      Content = content }: DOM.CodeBlock
                )
            | None, _ -> Error "Missing content element."
            | _, None -> Error "Missing style element."

        member code.WriteToJson(writer: Utf8JsonWriter) =
            writer.WriteString("type", "code")
            writePropertyObject code.Style.WriteToJson "style" writer

            writeArray
                (fun w ->
                    code.Content
                    |> List.map (fun c -> writeObject c.WriteToJson w)
                    |> ignore)
                "content"
                writer
                            
                
    type DOM.ListItem with
        static member FromJson(element: JsonElement) =
            match tryGetContent element, tryGetStyle element with
            | Some ce, Some se ->
                let (content, errors) =
                    ce
                    |> List.map DOM.InlineContent.FromJson
                    |> Utils.collectResults

                Ok(
                    { Style = DOM.Style.FromJsonWithDefault se
                      Content = content }: DOM.ListItem
                )
            | None, _ -> Error "Missing content element."
            | _, None -> Error "Missing style element."


        member item.WriteToJson(writer: Utf8JsonWriter) =
            writePropertyObject item.Style.WriteToJson "style" writer

            writeArray
                (fun w ->
                    item.Content
                    |> List.map (fun c -> writeObject c.WriteToJson w)
                    |> ignore)
                "content"
                writer

    type DOM.ListBlock with

        static member FromJson(element: JsonElement) =

            let ordered = tryGetBoolProperty "ordered" element
            let items = tryGetArrayProperty "items" element

            match ordered, items, tryGetStyle element with
            | Some o, Some i, Some se ->
                let (items, errors) =
                    i
                    |> List.map DOM.ListItem.FromJson
                    |> Utils.collectResults

                Ok(
                    { Style = DOM.Style.FromJsonWithDefault se
                      Ordered = o
                      Items = items }: DOM.ListBlock
                )
            | None, _, _ -> Error "Missing `ordered` element."
            | _, None, _ -> Error "Missing `items` element."
            | _, _, None -> Error "Missing `style` element."


        member list.WriteToJson(writer: Utf8JsonWriter) =
            writer.WriteString("type", "l")
            writer.WriteBoolean("ordered", list.Ordered)
            writePropertyObject list.Style.WriteToJson "style" writer

            writeArray
                (fun w ->
                    list.Items
                    |> List.map (fun c -> writeObject c.WriteToJson w)
                    |> ignore)
                "items"
                writer

    type DOM.ImageBlock with

        static member FromJson(element: JsonElement) = ()

        member item.WriteToJson(writer: Utf8JsonWriter) = ()

    type DOM.BlockContent with
        static member FromJson(element: JsonElement) =
            // try
            match tryGetType element with
            | Some t when t = "h" ->
                match DOM.HeaderBlock.FromJson element with
                | Ok h -> Ok <| DOM.BlockContent.Header h
                | Error e -> Error $"Could not create header block. Error: \"{e}\""
            | Some t when t = "p" ->
                match DOM.ParagraphBlock.FromJson element with
                | Ok p -> Ok <| DOM.BlockContent.Paragraph p
                | Error e -> Error $"Could not create paragraph block. Error: \"{e}\""
            | Some t when t = "code" ->
                match DOM.CodeBlock.FromJson element with
                | Ok c -> Ok <| DOM.Code c
                | Error e -> Error $"Could not create code block. Error: \"{e}\""
            | Some t when t = "l" ->
                match DOM.ListBlock.FromJson element with
                | Ok l -> Ok <| DOM.BlockContent.List l
                | Error e -> Error $"Could not create list block. Error: \"{e}\""
            | Some t when t = "i" -> Error "Image serialization not implemented yet." // TODO Implement.
            | Some t -> Error $"Unknown block type '{t}'"
            | None -> Error "Missing `type` property."

        member content.WriteToJson(writer: Utf8JsonWriter) =
            match content with
            | DOM.BlockContent.Header h -> h.WriteToJson writer
            | DOM.BlockContent.Paragraph p -> p.WriteToJson writer
            | DOM.BlockContent.Code c -> c.WriteToJson writer
            | DOM.BlockContent.List l -> l.WriteToJson writer
            | DOM.BlockContent.Image i -> ()

    type DOM.Section with

        static member FromJson(element: JsonElement) =
            let style = tryGetStyle element
            let title = None
            let name = tryGetName element
            let content = tryGetContent element

            match style, name, content with
            | Some se, Some n, Some ce ->
                let (content, errors) =
                    ce
                    |> List.map DOM.BlockContent.FromJson
                    |> Utils.collectResults
                Ok(
                    { Name = n
                      Title = title
                      Style = DOM.Style.FromJsonWithDefault se
                      Content = content }: DOM.Section
                )
            | None, _, _ -> Error "Missing `style` property."
            | _, None, _ -> Error "Missing `name` property."
            | _, _, None -> Error "Missing `content` property."

        member section.WriteToJson(writer : Utf8JsonWriter) =
            writer.WriteString("name", section.Name)
            
            // TODO add title block
            writePropertyObject section.Style.WriteToJson "style" writer

            writeArray
                (fun w ->
                    section.Content
                    |> List.map (fun c -> writeObject c.WriteToJson w)
                    |> ignore)
                "content"
                writer
                
    type DOM.Document with
        
        static member FromJson(element : JsonElement) =
            let style = tryGetStyle element
            let title = None
            let name = tryGetName element
            let content = tryGetArrayProperty "sections" element

            match style, name, content with
            | Some se, Some n, Some ce ->
                let (sections, errors) =
                    ce
                    |> List.map DOM.Section.FromJson
                    |> Utils.collectResults
                Ok(
                    { Name = n
                      Title = title
                      Style = DOM.Style.FromJsonWithDefault se
                      Sections = sections
                      Resources = [] }: DOM.Document // TODO add serialization for resources.
                )
            | None, _, _ -> Error "Missing `style` property."
            | _, None, _ -> Error "Missing `name` property."
            | _, _, None -> Error "Missing `sections` property."
             
        member document.WriteToJson(writer : Utf8JsonWriter) =
            writer.WriteString("name", document.Name)
            
            // TODO add title block
            writePropertyObject document.Style.WriteToJson "style" writer

            writeArray
                (fun w ->
                    document.Sections
                    |> List.map (fun c -> writeObject c.WriteToJson w)
                    |> ignore)
                "sections"
                writer    
            
type Serializer() =

    static member Deserialize(json: string) =
        let json = (JsonDocument.Parse json).RootElement
        DOM.Document.FromJson(json)
        
    static member Serialize(stream: Stream, document: DOM.Document) =
        let mutable options = JsonWriterOptions()
        options.Indented <- true
        use writer = new Utf8JsonWriter(stream, options)
        writer.WriteStartObject()
        document.WriteToJson(writer)
        writer.WriteEndObject()
    
    static member WriteToFile(document: DOM.Document, path: string) =
        let fs = File.OpenWrite(path)
        Serializer.Serialize(fs, document)

    static member AsBytes(document: DOM.Document, path: string) =
        use ms = new MemoryStream()
        Serializer.Serialize(ms, document)
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        ms.ToArray