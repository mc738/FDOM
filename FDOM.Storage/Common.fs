module FDOM.Storage.Common

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
module Sqlite =

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