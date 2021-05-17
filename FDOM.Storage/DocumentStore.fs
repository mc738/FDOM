namespace FDOM.Storage

open System
open System.IO
open System.Security.Cryptography
open FDOM.Core
open FDOM.Core.Common
open FDOM.Storage.Documents.Internal
open FLite.Core
open FDOM.Storage.Blobs
open FDOM.Storage.Resources
open FDOM.Storage.Documents

type DocumentStore(qh: QueryHandler) =

    let blobStore = BlobStore(qh, SHA256.Create())
    let resourceHandler = ResourceHandler(qh)
    let documentHandler = DocumentHandler(qh)

    static member Create(path) =
        let qh = QueryHandler.Create(path)
        let ds = DocumentStore(qh)
        ds.Initialize()
        ds
    
    static member Open(path) =
        let qh = QueryHandler.Open(path)
        DocumentStore(qh)
        
    member ds.Initialize() =
        blobStore.Initialize() |> ignore
        resourceHandler.Initialize()
        documentHandler.Initialize()

    member ds.AddResource() = ()

    member ds.AddDocumentVersion
        (
            docRef: Guid,
            document: DOM.Document,
            major: int,
            minor: int,
            revision: int,
            suffix: string,
            renderedDocuments: DOM.RenderedDocument list
        ) =

        use ms = new MemoryStream()

        let filePath = $"{document.SnakeCaseName}/{major}_{minor}_{revision}"
        
        Serialization.Serializer.Serialize(ms, document)

        let blobRef =
            blobStore.AddBlob("documents", $"{document.SnakeCaseName}_fdom", ".json", ms)

        blobStore.AddVirtualFile($"{filePath}/.artifacts/{document.SnakeCaseName}_fdom.json", blobRef)
        
        let versionRef =
            documentHandler.AddDocumentVersion(docRef, major, minor, revision, suffix, blobRef)

        // TODO handle import errors.
        // Add resources.
        let (_, resourceErrs) =
            document.Resources
            |> List.map
                (fun r ->
                    match blobStore.ImportFile("resources", r.Path) with
                    | Ok ref ->
                        // Add the resource and version resource link.
                        let resourceRef =
                            resourceHandler.AddResource(r.Name, $"{filePath}/{r.VirtualPath}", ref)

                        documentHandler.AddDocumentVersionResource(versionRef, resourceRef)
                        blobStore.AddVirtualFile($"{filePath}/{r.VirtualPath}", ref)
                        Ok()
                    | Error e -> Error e)
            |> Utils.collectResults

        // Add rendered documents.
        let (_, renderedDocumentErrs) =
            renderedDocuments
            |> List.map
                (fun rd ->
                    match blobStore.ImportFile("rendered_documents", rd.Path) with
                    | Ok ref ->
                        documentHandler.AddRenderedDocument(versionRef, ref)
                        blobStore.AddVirtualFile($"{filePath}/{rd.VirtualPath}", ref)
                        Ok()
                    | Error e -> Error e)
            |> Utils.collectResults

        versionRef

    /// Add a new document.
    member ds.AddDocument(document: DOM.Document, isDraft: bool, renderedDocuments: DOM.RenderedDocument list) =
        // Serialize the document to json and add as a blob.
        //use ms = new MemoryStream()

        //let filePath = $"{document.SnakeCaseName}/{major}_{minor}_{revision}"
        
        //Serialization.Serializer.Serialize(ms, document)

        //let blobRef =
        //    blobStore.AddBlob("documents", $"{document.Name}_fdom", ".json", ms)

        // Add the document and an initial version.
        // TODO this is not proper url encoding.
        let docRef =
            documentHandler.AddDocument(document.Name, document.SnakeCaseName)
        
        let (major, minor, suffix) =
            match isDraft with
            | true -> 0, 1, "draft"
            | false -> 1, 0, ""

        ds.AddDocumentVersion(docRef, document, major, minor, 0, suffix, renderedDocuments) |> ignore
        docRef

    /// Get the latest version of a document.
    member ds.GetDocument() = ()

    /// Get a specific version of a document.
    member ds.GetDocumentVersion() = ()

    member ds.GetResource() = ()

    member ds.AddBlob() = ()

    member ds.GetBlob() = ()

    member ds.GetDuplicateBlobs() = blobStore.GetDuplicateBlobs()

    member ds.GetVirtualFile(path) = blobStore.GetVirtualFile(path)