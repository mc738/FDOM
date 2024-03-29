﻿namespace FDOM.Rendering

open System
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

    let tryDeserializeColor (value: string) =
        let getByte (values: string array) (index: int) =
            values
            |> Array.tryItem index
            |> Option.bind (fun v ->
                match System.Byte.TryParse v with
                | true, r -> Some r
                | false, _ -> None)
            |> Option.defaultValue 0uy


        match value with
        | _ when value.StartsWith '#' -> None
        | _ when value.StartsWith "rgba" ->
            let values = value.Replace("rgba(", "").Replace(")", "").Split(',')

            let get = getByte values

            Style.Color.RGBA(get 0, get 1, get 2, get 3) |> Some
        | _ when value.StartsWith "rgb(" ->
            let values = value.Replace("rgb(", "").Replace(")", "").Split(',')

            let get = getByte values

            Style.Color.RGBA(get 0, get 1, get 2, 0uy) |> Some
        | _ when value.StartsWith "hsl" -> None
        | _ -> Style.Color.Named value |> Some

    let tryDeserializeUnit (value: string) =
        //match value.EndsWith(v) when
        //| v is "" -> ()
        let tryParse (str: string) =
            match Double.TryParse str with
            | true, v -> Some v
            | false, _ -> None


        match value with
        | _ when value.EndsWith("px") ->
            // Pixel to mm -> 1 pixel (X) = 0.2645833333 mm
            value.Replace("px", "")
            |> tryParse
            |> Option.map ((*) 0.2645833333)
            |> Option.map Style.Unit.Millimeter
        | _ when value.EndsWith("cm") -> value.Replace("cm", "") |> tryParse |> Option.map Style.Unit.Centimeter
        | _ when value.EndsWith("mm") -> value.Replace("mm", "") |> tryParse |> Option.map Style.Unit.Millimeter
        | _ when value.EndsWith("in") -> value.Replace("in", "") |> tryParse |> Option.map Style.Unit.Inch
        | _ when value.EndsWith("pt") -> value.Replace("pt", "") |> tryParse |> Option.map Style.Unit.Point
        | _ when value.EndsWith("pi") -> value.Replace("pi", "") |> tryParse |> Option.map Style.Unit.Pica
        | _ -> None
