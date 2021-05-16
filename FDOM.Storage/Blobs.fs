namespace FDOM.Storage.Blobs

open System
open System.IO
open System.Security.Cryptography
open System.Text
open FLite.Core

module private Helpers =
    let bytesToHex (bytes: byte array) =
        bytes
        |> Array.fold (fun (sb: StringBuilder) b -> sb.AppendFormat("{0:x2}", b)) (StringBuilder(bytes.Length * 2))
        |> fun sb -> sb.ToString()

    let generateHash (hasher: SHA256) (bytes: byte array) = hasher.ComputeHash bytes |> bytesToHex

    let hashStream (hasher: SHA256) (stream: Stream) =
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        let hash = hasher.ComputeHash stream |> bytesToHex
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        hash

type NewBlob =
    { Reference: Guid
      Bucket: string
      Name: string
      Ext: string
      Raw: BlobField
      Hash: string
      CreatedOnUtc: DateTime }
    static member Create(bucket: string, name: string, ext: string, data: Stream, hasher: SHA256) =
        let hash = Helpers.hashStream hasher data

        { Reference = Guid.NewGuid()
          Bucket = bucket
          Name = name
          Ext = ext
          Raw = BlobField.FromStream data
          Hash = hash
          CreatedOnUtc = DateTime.UtcNow }

type BlobOverview =
    { Reference: Guid
      Bucket: string
      Name: string
      Ext: string }

type GetBlobQuery = { BlobRef: Guid }

type Blob =
    { Reference: Guid
      Name: string
      Bucket: string
      Raw: BlobField
      Hash: string
      ContentTypeName: string
      Ext: string
      HttpContentType: string
      CreatedOnUtc: DateTime }

    member blob.ToBytes() = blob.Raw.ToBytes()

    member blob.SaveAs(path: string) =
        let fs =
            File.OpenWrite(Path.Combine(path, $"{blob.Name}{blob.Ext}"))

        blob.Raw.Value.CopyTo fs
        Ok()

    member blob.Save(path: string) = blob.SaveAs(path)

module Internal =

    let initializeContentTypes = """
    CREATE TABLE content_types (
	    name TEXT NOT NULL,
        ext TEXT NOT NULL,
	    http_content_type TEXT NOT NULL,
	    CONSTRAINT content_types_PK PRIMARY KEY (ext)
    );
    """

    let initializeBlobStore = """
    CREATE TABLE blob_store (
	    reference TEXT NOT NULL,
	    bucket TEXT NOT NULL,
	    name TEXT NOT NULL,
        ext TEXT NOT NULL,
	    "raw" BLOB NOT NULL,
	    hash TEXT NOT NULL,
        created_on_utc TEXT NOT NULL,
	    CONSTRAINT blob_store_PK PRIMARY KEY (reference),
        CONSTRAINT blob_store_FK FOREIGN KEY (ext) REFERENCES content_types(ext)
    );
    """

    let initializeVirtualFileSystem = """
    CREATE TABLE virtual_fs (
	    file_path TEXT NOT NULL,
        blob_reference TEXT NOT NULL
	    CONSTRAINT virtual_fs_PK PRIMARY KEY (file_path),
        CONSTRAINT virtual_fs_FK FOREIGN KEY (blob_reference) REFERENCES blob_store(reference)
    );
    """

    type ContentType =
        { Name: string
          Ext: string
          HttpContentType: string }

    let seedContentTypes =
        [ { Name = "Text"
            Ext = ".txt"
            HttpContentType = "text/plain" }
          { Name = "Css"
            Ext = ".css"
            HttpContentType = "text/css" }
          { Name = "Html"
            Ext = ".html"
            HttpContentType = "text/html" }
          { Name = "Javascript"
            Ext = ".js"
            HttpContentType = "text/javascript" }
          { Name = "Apng"
            Ext = ".apng"
            HttpContentType = "image/apng" }
          { Name = "Avif"
            Ext = ".avif"
            HttpContentType = "image/avif" }
          { Name = "Gif"
            Ext = ".gif"
            HttpContentType = "image/gif" }
          { Name = "Jpeg"
            Ext = ".jpg"
            HttpContentType = "image/jpeg" }
          { Name = "Png"
            Ext = ".png"
            HttpContentType = "image/png" }
          { Name = "Svg"
            Ext = ".svg"
            HttpContentType = "image/svg+xml" }
          { Name = "Webp"
            Ext = ".webp"
            HttpContentType = "image/wepb" }
          { Name = "Json"
            Ext = ".json"
            HttpContentType = "application/json" }
          { Name = "Markdown"
            Ext = ".md"
            HttpContentType = "text/plain" } ]

    let initialize (qh: QueryHandler) =
        qh.ExecuteInTransaction
            (fun t ->
                t.ExecuteSqlNonQuery(initializeContentTypes)
                |> ignore

                t.ExecuteSqlNonQuery(initializeBlobStore)
                |> ignore

                t.InsertList("content_types", seedContentTypes))


type BlobStore(qh: QueryHandler, hasher: SHA256) =


    member bs.Initialize() = Internal.initialize qh

    member bs.AddBlob(bucket: string, name: string, ext: string, data: Stream) =
        let blob =
            NewBlob.Create(bucket, name, ext, data, hasher)

        qh.Insert("blob_store", blob)
        blob.Reference

    /// Import a file a a blob.
    member bs.ImportFile(bucket: string, path: string) =
        match File.Exists path with
        | true ->
            let fi = FileInfo(path)
            Ok (bs.AddBlob(bucket, Path.GetFileNameWithoutExtension path, fi.Extension, File.OpenRead(path)))
        | false -> Error $"File '{path}' could not be found."

    member bs.GetBlob(reference: Guid) =

        let sql = """
        SELECT
            b.reference,
            b.name,
            b.bucket,
            b."raw",
            b.hash,
            ct.name as content_type_name,
            ct.ext,
            ct.http_content_type,
            b.created_on_utc
        FROM blob_store b
        JOIN content_types ct ON b.ext = ct.ext
        WHERE b.reference = @blob_ref;
        """

        qh.SelectSingleVerbatim<Blob, GetBlobQuery>(sql, { BlobRef = reference })
