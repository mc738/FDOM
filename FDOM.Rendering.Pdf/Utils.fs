namespace FDOM.Rendering

open FDOM.Core.Common
open FreDF.Core

[<AutoOpen>]
module Utils =

    let deserializeColor (value: string) =
        let getByte (values: string array) (index: int) =
            values
            |> Array.tryItem index
            |> Option.bind (fun v ->
                match System.Byte.TryParse v with
                | true, r -> Some r
                | false, _ -> None)
            |> Option.defaultValue 0uy


        match value with
        | _ when value.StartsWith '#' ->
            // TODO convert hex to rgba
            failwith "Hex not currently supported"

        | _ when value.StartsWith "rgba" ->
            let values = value.Replace("rgba(", "").Replace(")", "").Split(',')

            let get = getByte values

            Style.Color.RGBA(get 0, get 1, get 2, get 3)
        | _ when value.StartsWith "rgb(" ->
            let values = value.Replace("rgb(", "").Replace(")", "").Split(',')

            let get = getByte values

            Style.Color.RGBA(get 0, get 1, get 2, 0uy)
        | _ when value.StartsWith "hsl" ->
            let values = value.Replace("hsl(", "").Replace(")", "").Split(',')

            failwith "Hsl colors not currently supported"
        | _ -> Style.Color.Named value

    let deserializeUnit (value: string) =
        ()