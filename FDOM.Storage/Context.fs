module FDOM.Storage.Context

open System
open System.Security.Cryptography
open FDOM.Core.Common
open Microsoft.Data.Sqlite


module Internal =
    
    
    
    type StorageAgent() =
        
        member a.PostCommand() = ()

type StorageContext(connect: SqliteConnection, hasher: SHA256) =

    static member Create(path: string) =
        use connection =
            new SqliteConnection($"Data Source={path};")

        connection.Open()

        StorageContext(connection, SHA256.Create())

    member context.Initialize() =

        ()

    member context.AddNewDocument(document: DOM.Document) =

        ()

    member context.AddDocumentVersion(documentRef: Guid, document: DOM.Document) =

        ()

    member context.AddResource() = ()

    member context.AddBlob() = ()

    member context.GetBlob(reference: Guid) = ()

    /// The equivalent of a standard unix file system path.
    /// For use when using the storage context as a replacement for a standard file system.
    member context.GetByPath(path: string) = ()

    /// The equivalent of a url.
    /// For use when storage context is being used for static web content.
    member context.GetByUrl(url: string) = ()
