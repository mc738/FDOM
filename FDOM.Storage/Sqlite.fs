module FDOM.Storage.Sqlite

open System
open System.IO
open System.Security.Cryptography
open System.Text
open FDOM.Core.Common
open Microsoft.Data.Sqlite

[<AutoOpen>]
module Common =
    type InitializationStatement =
        { Name: string
          Sql: string }
        
        member query.Execute(connection : SqliteConnection) =
            use comm = new SqliteCommand(query.Sql, connection)
            comm.ExecuteNonQuery() |> ignore
    
    type Query<'p, 'r> =
        { Name: string
          Sql: string
          ParameterMapper: ('p -> Map<string, obj>) option
          ResultMapper: SqliteDataReader -> 'r }
        member query.Prepare(connection: SqliteConnection, parameters: 'p) =
            connection.Open()
            use comm = new SqliteCommand(query.Sql, connection)

            match query.ParameterMapper with
            | Some pm ->
                pm parameters
                |> Map.map (fun k v -> comm.Parameters.AddWithValue(k, v))
                |> ignore
            | None -> ()

            comm.Prepare()

            comm

        member query.Execute(connection: SqliteConnection, parameters: 'p) =
            let comm = query.Prepare(connection, parameters)

            use reader = comm.ExecuteReader()

            let results = query.ResultMapper reader
            results

    type InsertBlobQuery<'p> =
        { Name: string
          Sql: string
          ParameterMapper: 'p -> Map<string, obj>
          TableName: string
          ColumnName: string }

        member query.Execute(connection: SqliteConnection, parameters: 'p, data: Stream, ?parameterName : string) =
            connection.Open()
            use comm = new SqliteCommand(query.Sql, connection)

            // Map other parameters.
            query.ParameterMapper parameters
            |> Map.map (fun k v -> comm.Parameters.AddWithValue(k, v))
            |> ignore
        
            let pName = defaultArg parameterName "length"
        
            // Add parameter for blob length    
            comm.Parameters.AddWithValue($"${pName}", data.Length) |> ignore
            
            comm.Prepare()

            // Get the newly added blob's id.
            let rowId = comm.ExecuteScalar() :?> int64

            // Write to the blob.
            use writeStream =
                new SqliteBlob(connection, query.TableName, query.ColumnName, rowId)

            data.CopyTo(writeStream)

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

module Internal =
    
    let insertResourceQuery = {
        Name = "insert_blob"
        Sql = """
        
        """
        ParameterMapper = (fun () -> Map.empty)
        TableName = "resources"
        ColumnName = "resource_blob"
    }

