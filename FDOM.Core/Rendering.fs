namespace FDOM.Core

module Rendering =
    
    open FDOM.Core.Common
    
    
    type IRenderer =

        abstract member Run: DOM.Document -> unit



