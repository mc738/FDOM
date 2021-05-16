namespace FDOM.Storage

open System.IO
open System.Security.Cryptography
open FDOM.Core
open FDOM.Core.Common
open FLite.Core
open FDOM.Storage.Blobs
open FDOM.Storage.Resources
open FDOM.Storage.Documents

type DocumentStore(qh: QueryHandler) =

    let blobStore = BlobStore(qh, SHA256.Create())
    let resourceHandler = ResourceHandler(qh)
    let documentHandler = DocumentHandler(qh)

    member ds.Initialize() =
        blobStore.Initialize() |> ignore
        resourceHandler.Initialize()
        documentHandler.Initialize()

    member ds.AddResource() = ()

    /// Add a new document.
    member ds.AddDocument(document: DOM.Document, isDraft: bool, renderedDocuments: string list) =
        // Serialize the document to json and add as a blob.
        use ms = new MemoryStream()

        Serialization.Serializer.Serialize(ms, document)

        let blobRef =
            blobStore.AddBlob("documents", $"{document.Name}_fdom", ".json", ms)

        // Add the document and an initial version.
        // TODO this is not proper url encoding.
        let docRef =
            documentHandler.AddDocument(document.Name, document.Name.Replace(' ', '_'))

        let (major, minor, suffix) =
            match isDraft with
            | true -> 0, 1, "draft"
            | false -> 1, 0, ""

        let versionRef =
            documentHandler.AddDocumentVersion(docRef, major, minor, 0, suffix, blobRef)

        // TODO handle import errors.
        // Add resources.
        let (_, resourceErrs) =
            document.Resources
            |> List.map (fun r ->
                match blobStore.ImportFile("resources", r.Path) with
                | Ok ref ->
                    // Add the resource and version resource link.
                    let resourceRef = resourceHandler.AddResource(r.Name, r.VirtualPath, ref)
                    documentHandler.AddDocumentVersionResource(versionRef, resourceRef)
                    Ok ()
                | Error e -> Error e)
            |> Utils.collectResults

        // Add rendered documents.
        let (_, renderedDocumentErrs) =
            renderedDocuments
            |> List.map (fun rd ->
                match blobStore.ImportFile("rendered_documents", rd) with
                | Ok ref -> Ok (documentHandler.AddRenderedDocument(versionRef, ref))
                | Error e -> Error e
                )
            |> Utils.collectResults
            
        ()    
        

    member ds.AddDocumentVersion() = ()

    /// Get the latest version of a document.
    member ds.GetDocument() = ()

    /// Get a specific version of a document.
    member ds.GetDocumentVersion() = ()

    member ds.GetResource() = ()

    member ds.AddBlob() = ()

    member ds.GetBlob() = ()
