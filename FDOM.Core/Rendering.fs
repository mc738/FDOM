namespace FDOM.Core

module Rendering =
    
    open FDOM.Core.Common
    
    
    
    type Preprocessor =
        {
            Name: string
            DocumentRewriter: DOM.Document -> DOM.Document
            
        }
    
    type IRenderer =

        abstract member Run: DOM.Document -> unit



