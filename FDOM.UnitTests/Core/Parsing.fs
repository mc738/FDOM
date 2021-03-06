module FDOM.UnitTests.Core.Parsing

open FDOM.Core.Common
open FDOM.Core.Common
open FDOM.Core.Parsing
open FDOM.Core.Parsing.BlockParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type BlockParsing () =

    [<TestMethod>]
    member this.``Parse paragraph single line`` () =
        
        let input = Input.Create(["Hello, World!"])
        
        let expected = [ BlockToken.Paragraph "Hello, World!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
    
    [<TestMethod>]    
    member this.``Parse h1 single line`` () =
        
        let input = Input.Create(["# Hello, World!"])
        
        let expected = [ BlockToken.Header "# Hello, World!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
        
    [<TestMethod>]    
    member this.``Parse h2 single line`` () =
        
        let input = Input.Create(["## Hello, World!"])
        
        let expected = [ BlockToken.Header "## Hello, World!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
        
    [<TestMethod>]    
    member this.``Parse ordered list single line`` () =
        
        let input = Input.Create(["1. Hello, World!"])
        
        let expected = [ BlockToken.OrderListItem "Hello, World!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)

    [<TestMethod>]    
    member this.``Parse ordered list double digit single line`` () =
        
        let input = Input.Create(["99. Hello, World!"])
        
        let expected = [ BlockToken.OrderListItem "Hello, World!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)

    [<TestMethod>]    
    member this.``Parse unordered list single line`` () =
        
        let input = Input.Create(["* Hello, World!"])
        
        let expected = [ BlockToken.UnorderedListItem "Hello, World!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
        
    [<TestMethod>]    
    member this.``Parse unordered list 2 lines`` () =
        
        let input = Input.Create(["* Hello, "; "World!" ])
        
        let expected = [ BlockToken.UnorderedListItem "Hello, World!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
            
    [<TestMethod>]    
    member this.``Parse code block`` () =
        
        let input = Input.Create(["```"; "let msg = \"Hello, World!\""; "```" ])
        
        let expected = [ BlockToken.CodeBlock "let msg = \"Hello, World!\"" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
        
    [<TestMethod>]    
    member this.``Parse header and paragraph`` () =
        
        let input = Input.Create(["# Hello, World!"; ""; "This is some text!" ])
        
        let expected = [ BlockToken.Header "# Hello, World!"; BlockToken.Paragraph "This is some text!" ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
   
    [<TestMethod>]    
    member this.``Parse 2 paragraph with double empty line`` () =
        
        let input = Input.Create(["Paragraph 1."; ""; ""; "Paragraph 2." ])
        
        let expected = [ BlockToken.Paragraph "Paragraph 1."; BlockToken.Empty; BlockToken.Paragraph "Paragraph 2." ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
   
    [<TestMethod>]    
    member this.``Parse 2 unordered list items`` () =
        
        let input = Input.Create(["* Item 1."; "* Item 2."; ])
        
        let expected = [ BlockToken.UnorderedListItem "Item 1."; BlockToken.UnorderedListItem "Item 2." ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)    
        
        
    [<TestMethod>]    
    member this.``Parse 2 ordered list items`` () =
        
        let input = Input.Create(["1. Item 1."; "2. Item 2."; ])
        
        let expected = [ BlockToken.OrderListItem "Item 1."; BlockToken.OrderListItem "Item 2." ]
        
        let actual = parseBlocks input
        
        Assert.AreEqual(expected, actual)
        
[<TestClass>]
type InlineParsing () =
    
    [<TestMethod>]
    member this.``Parse bold content`` () =
        
        let input = "Hello, **World**!"
        
        let expected = [
            DOM.InlineContent.Text { Content = "Hello, " }
            DOM.InlineContent.Span { Content = "World"; Style = DOM.Style.Ref [ "b" ]  }
            DOM.InlineContent.Text { Content = "!" }
        ]
        
        let actual = InlineParser.parseInlineContent input
        
        Assert.AreEqual(expected, actual)

    [<TestMethod>]
    member this.``Parse italic content`` () =
        
        let input = "Hello, *World*!"
        
        let expected = [
            DOM.InlineContent.Text { Content = "Hello, " }
            DOM.InlineContent.Span { Content = "World"; Style = DOM.Style.Ref [ "i" ]  }
            DOM.InlineContent.Text { Content = "!" }
        ]
        
        let actual = InlineParser.parseInlineContent input
        
        Assert.AreEqual(expected, actual)
    
    
    [<TestMethod>]
    member this.``Parse bold italic content`` () =
        
        let input = "Hello, ***World***!"
        
        let expected = [
            DOM.InlineContent.Text { Content = "Hello, " }
            DOM.InlineContent.Span { Content = "World"; Style = DOM.Style.Ref [ "b"; "i" ]  }
            DOM.InlineContent.Text { Content = "!" }
        ]
        
        let actual = InlineParser.parseInlineContent input
        
        Assert.AreEqual(expected, actual)
    
    [<TestMethod>]    
    member this.``Parse inline code`` () =
        
        let input = "Hello, `World`!"
        
        let expected = [
            DOM.InlineContent.Text { Content = "Hello, " }
            DOM.InlineContent.Span { Content = "World"; Style = DOM.Style.Ref [ "code" ]  }
            DOM.InlineContent.Text { Content = "!" }
        ]
        
        let actual = InlineParser.parseInlineContent input
        
        Assert.AreEqual(expected, actual)