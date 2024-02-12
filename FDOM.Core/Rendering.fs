namespace FDOM.Core

module Rendering =
    
    open FDOM.Core.Common
    
    type [<RequireQualifiedAccess>] PreprocessorType =
        | DocumentRewriter of (DOM.Document -> DOM.Document)
        | Bespoke of (unit -> Result<unit, string>)
    
    type Preprocessor =
        {
            Name: string
            DocumentRewriter: DOM.Document -> DOM.Document
        }
    
    type IRenderer =

        abstract member Run: DOM.Document -> unit



