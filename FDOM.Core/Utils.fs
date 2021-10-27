namespace FDOM.Core

[<RequireQualifiedAccess>]
/// A collection of utility functions. In time these can be moved to `FUtil` or replaced with the equivalent. 
module Utils =
    
    /// A helper function to separate a list of Result<'Type, 'Error>.
    ///
    /// ## Returns
    /// A tuple comprised of list of `'Type` and a list of `'Error`
    ///
    /// ## Example
    /// ```
    /// let results: Result<'Type, 'Error> list = getResults
    /// let (items, errors) = collectResults results
    /// ```
    let collectResults (results: Result<'Type, 'Error> list) =
        results
        |> List.fold
            (fun (ok, err) r ->
                match r with
                | Ok r -> (ok @ [ r ], err)
                | Error e -> (ok, err @ [ e ]))
            ([], [])