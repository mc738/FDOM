namespace FDOM.Storage

open System
open System.IO
open FDOM.Storage.Common.Sqlite
open Microsoft.Data.Sqlite
open FDOM.Storage.Sqlite

type Blob =
    { Reference: Guid
      Name: string
      Bucket: string
      Data: Stream
      Hash: string
      Extension: string }
    static member FromReader(reader: SqliteDataReader) =
            [ while reader.Read() do
                  { Reference = reader.GetGuid(0)
                    Name = reader.GetString(1)
                    Bucket = reader.GetString(2)
                    Data = reader.GetStream(3)
                    Hash = reader.GetString(4)
                    Extension = reader.GetString(5) } ]
            
    static member SingleFromReader(reader : SqliteDataReader) =
        let result = Blob.FromReader reader
        
        match result.Length with
        | 0 -> None
        | 1 -> Some result.Head
        | _ -> Some result.Head
    
    member blob.ToBytes() =
        use ms = new MemoryStream()
        blob.Data.CopyTo ms
        ms.ToArray()
    
    member blob.SaveAs(path : string, name : string) =
        let fs =  File.OpenWrite(Path.Combine(path, $"{name}{blob.Extension}"))
        blob.Data.CopyTo fs
        Ok ()
        
    member blob.Save(path : string) = blob.SaveAs(path, blob.Name)

    


module Initialization =

    let initializeBlobStore =
        ({ Name = "initialize_blob_store"
           Sql = """
            CREATE TABLE blob_store (
	            reference TEXT NOT NULL,
	            bucket TEXT NOT NULL,
	            name TEXT NOT NULL,
	            "raw" BLOB NOT NULL,
	            hash TEXT NOT NULL,
                extension TEXT,
	            CONSTRAINT blob_store_PK PRIMARY KEY (reference)
            );""" }: InitializationStatement)

    let i = ()

module Queries =

    type NewBlobParameters = {
        Reference: string
        Name: string
        Bucket: string
        Hash: string
        Extension: string
    }

    let addBlob =
        ({ Name = "add_blob"
           Sql = """

        """
           ParameterMapper = fun (p : NewBlobParameters) -> Map.ofList [
               "", p.Reference :> obj
               "", p.Name :> obj
               "", p.Bucket :> obj
               "", p.Hash :> obj
               "", p.Extension :> obj
           ]
           TableName = "blob_store"
           ColumnName = "raw" }: InsertBlobQuery<NewBlobParameters>)

    let getBlobByRef =
        ({ Name = "get_blob_by_ref"
           Sql = """

        """
           ParameterMapper = Some(fun (reference: Guid) -> Map.ofList [ "@ref", reference :> obj ])
           ResultMapper = Blob.SingleFromReader }: Query<Guid, Blob option>)

type BlobStore(connection: SqliteConnection) =

    let i = ()
