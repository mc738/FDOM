module FDOM.Storage.Sqlite

open System.IO
open System.Security.Cryptography
open System.Text
open Microsoft.Data.Sqlite

module private Helpers =
    let bytesToHex (bytes: byte array) =
        bytes
        |> Array.fold (fun (sb: StringBuilder) b -> sb.AppendFormat("{0:x2}", b)) (StringBuilder(bytes.Length * 2))
        |> fun sb -> sb.ToString()

    let generateHash (hasher: SHA256) (bytes: byte array) = hasher.ComputeHash bytes |> bytesToHex

[<AutoOpen>]
module Common =
    type InitializationStatement = { Name: string; Sql: string }

    type ArchivingContext =
        { BasePath: string
          Connection: SqliteConnection
          Hasher: SHA256 }
        static member Create(basePath: string, path: string) =
            use connection =
                new SqliteConnection(sprintf "Data Source=%s;" path)

            connection.Open()

            { BasePath = basePath
              Connection = connection
              Hasher = SHA256.Create() }



[<RequireQualifiedAccess>]
module InitializationStatements =

    let filesTable =
        { Name = "Create files table."
          Sql = """
            CREATE TABLE files (
	            name TEXT NOT NULL,
	            "path" TEXT NOT NULL,
	            extension TEXT NOT NULL,
	            "size" INTEGER NOT NULL,
	            hash INTEGER NOT NULL,
	            blob_data BLOB NOT NULL
            );
            """ }

    let directoriesTable =
        { Name = "Create directories table."
          Sql = """
            CREATE TABLE directories (
	            name TEXT NOT NULL,
	            "path" TEXT NOT NULL,
	            hash TEXT NOT NULL
            );
            """ }

    let keyValuesTable =
        { Name = "Create key values table."
          Sql = """
            CREATE TABLE key_values (
	            "key" TEXT NOT NULL,
	            value TEXT NOT NULL
            );
            """ }

    let defaultStatements =
        [ filesTable
          directoriesTable
          keyValuesTable ]

module internal Internal =

    let addFileBlob (context: ArchivingContext) (file: FileInfo) (data: byte array) hash =

        let insertFileSql = """
            INSERT INTO files
            (name, "path", extension, "size", hash, blob_data)
            VALUES($name, $path, $extension, $size, $hash, (zeroblob($length)));

            SELECT last_insert_rowid();
            """

        use comm =
            new SqliteCommand(insertFileSql, context.Connection)

        comm.Parameters.AddWithValue("$name", file.Name)
        |> ignore

        comm.Parameters.AddWithValue("$path", Path.GetRelativePath(context.BasePath, file.FullName))
        |> ignore

        comm.Parameters.AddWithValue("$extension", Path.GetExtension(file.FullName))
        |> ignore

        comm.Parameters.AddWithValue("$size", data.Length)
        |> ignore

        comm.Parameters.AddWithValue("$hash", hash)
        |> ignore

        comm.Parameters.AddWithValue("$length", data.Length)
        |> ignore

        comm.Prepare()
        let rowId = comm.ExecuteScalar() :?> int64

        use ms = new MemoryStream(data)

        use writeStream =
            new SqliteBlob(context.Connection, "files", "blob_data", rowId)

        ms.CopyTo(writeStream)

    let processFile (context: ArchivingContext) (file: FileInfo) =
        // Load and hash file
        printf "\tProcessing file '%s': " file.Name
        let bytes = File.ReadAllBytes file.FullName

        let hash =
            Helpers.generateHash context.Hasher bytes

        // Insert file into db.
        addFileBlob context file bytes hash
        printfn "\u2705  "
        hash

    type AddDirectoryParameters =
        { Name: string
          Path: string
          Hash: string }

    let rec processDirectory (context: ArchivingContext) (directory: DirectoryInfo) =

        printfn
            "Processing directory '%s' (%s)"
            directory.Name
            (Path.GetRelativePath(context.BasePath, directory.FullName))

        let fileHashes =
            directory.GetFiles()
            |> List.ofSeq
            |> List.sortBy (fun fi -> fi.Name)
            |> List.map (processFile context)

        let directoryHashes =
            directory.GetDirectories()
            |> List.ofSeq
            |> List.sortBy (fun di -> di.Name)
            |> List.map (processDirectory context)

        let hash =
            List.concat [ fileHashes
                          directoryHashes ]
            |> List.fold (fun (sb: StringBuilder) s -> sb.Append(s)) (StringBuilder())
            |> (fun sb -> sb.ToString())
            |> Encoding.UTF8.GetBytes
            |> (Helpers.generateHash context.Hasher)

        let insertDirectorySql = """
            INSERT INTO directories
            (name, "path", hash)
            VALUES($name, $path, $hash);
            """

        use comm =
            new SqliteCommand(insertDirectorySql, context.Connection)

        comm.Parameters.AddWithValue("$name", directory.Name)
        |> ignore

        comm.Parameters.AddWithValue("$path", Path.GetRelativePath(context.BasePath, directory.FullName))
        |> ignore

        comm.Parameters.AddWithValue("$hash", hash)
        |> ignore

        comm.Prepare()
        let rowId = comm.ExecuteNonQuery()

        printfn "Result for directory '%s':\u2705  " directory.Name
        hash

    let handleDirectory (context: ArchivingContext) =
        match Directory.Exists context.BasePath with
        | true -> processDirectory context (DirectoryInfo(context.BasePath))
        | false -> ""

    let initializeArchive (context: ArchivingContext) (additionalStatements: InitializationStatement list) =
        context.Connection.Open()

        List.concat [ InitializationStatements.defaultStatements
                      additionalStatements ]
        |> List.map (fun statement ->
            printf "Executing '%s 'initialize sql: " statement.Name

            use comm =
                new SqliteCommand(statement.Sql, context.Connection)

            comm.ExecuteNonQuery() |> ignore
            printfn "\u2705  ")
        |> ignore

    let createArchive (directoryPath: string) (outputPath: string) =
        File.WriteAllBytes(outputPath, [||])

        let context =
            ArchivingContext.Create(directoryPath, outputPath)

        initializeArchive context []

        printfn "Archive hash: %s" (handleDirectory context)
        context

    let createDirectories (context: ArchivingContext) (outputPath: string) =
        let sql = """
            SELECT "path"
            FROM directories;
            """

        use comm =
            new SqliteCommand(sql, context.Connection)

        let reader = comm.ExecuteReader()

        [ while reader.Read() do
            let path =
                Path.Combine(outputPath, reader.GetString(0))

            Directory.CreateDirectory path ]
        |> ignore

    let createFiles (context: ArchivingContext) (outputPath: string) =
        let sql = """
            SELECT "path", blob_data
            FROM files;
            """

        use comm =
            new SqliteCommand(sql, context.Connection)

        let reader = comm.ExecuteReader()

        [ while reader.Read() do
            let path =
                Path.Combine(outputPath, reader.GetString(0))

            let stream = reader.GetStream(1)
            let fileStream = File.Create(path)
            stream.CopyTo(fileStream) ]
        |> ignore

    let extractArchive (context: ArchivingContext) outputPath =
        printfn "Extracting to '%s'" outputPath
        Directory.CreateDirectory(outputPath) |> ignore
        createDirectories context outputPath
        createFiles context outputPath

type Archive =
    { Path: string
      Context: ArchivingContext }

    static member Create(directoryPath: string, outputPath: string) =
        { Path = directoryPath
          Context = Internal.createArchive directoryPath outputPath }


    member archive.Extract(path: string) =
        Internal.extractArchive archive.Context path
        
        