// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main argv =

    let matches = Regex.Matches("", "")
    
    printfn "Result: %A" matches
    
    0 // return an integer exit code
