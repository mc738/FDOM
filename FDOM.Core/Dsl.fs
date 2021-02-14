module FDOM.Core.Dsl

open FDOM.Core.Common

[<RequireQualifiedAccess>]
module Style =
    
    let custom items =  DOM.Style.Custom (Map.ofList items)
    
    let references items = DOM.Style.Ref items
    
    let none = DOM.Style.Default
 
module General =   
    let h1 indexed style content = DOM.createH1 indexed style content

    let h2 indexed style content = DOM.createH2 indexed style content

    let h3 indexed style content = DOM.createH3 indexed style content

    let h4 indexed style content = DOM.createH4 indexed style content

    let h5 indexed style content = DOM.createH5 indexed style content

    let h6 indexed style content = DOM.createH6 indexed style content

    let p style content = DOM.createParagraph style content

    let ol style items = DOM.createUnorderedList style items

    let ul style items = DOM.createOrderedList style items

    let li style content = DOM.createListItem style content

    let section name title style content =
        DOM.createSection style title name content

    let text content = DOM.createText content

    let span style content = DOM.createSpan style content

    let document name title style sections =
        DOM.createDocument style title name sections
    

/// A dsl for creating articles.    
module Article =
    let h1 content = General.h1 true (Style.references []) content

    let h2 content = General.h2 false (Style.references []) content

    let h3 content = General.h3 false (Style.references []) content

    let h4 content = General.h4 false (Style.references []) content

    let h5 content = General.h5 false (Style.references []) content

    let h6 content = General.h6 false (Style.references []) content

    let p content = General.p (Style.references []) content

    let code content = General.p (Style.references [ "code" ]) content
    
    let quote content = General.p (Style.references [ "quote" ]) content
    
    let ol items = General.ol (Style.references []) items

    let ul items = General.ul (Style.references []) items

    let li content = General.li (Style.references []) content

    let section name title style content =
        DOM.createSection style title name content

    let text content = DOM.createText content

    let span style content = DOM.createSpan style content
    
    let hl content = span (Style.references [ "hl" ]) content
    
    let bold content = span (Style.references [ "bold" ]) content

    let document name title style sections =
        DOM.createDocument style title name sections
