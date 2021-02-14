module FDOM.IntegrationTests.Core.Parsing

open System.IO
open FDOM.Core.Parsing.BlockParser
open Microsoft.VisualStudio.TestTools.UnitTesting

[<TestClass>]
type BlockParsing () =

    [<TestMethod>]
    member this.``Parse test article 1`` () =
        
        let input = Input.Create(File.ReadAllLines("test_article_1.md") |> List.ofArray)
        
        let expected =
            [
                BlockToken.Header "# Hello, World!"
                BlockToken.Paragraph "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Quisque non eleifend diam."
                BlockToken.Header "## The story continues..."
                BlockToken.Paragraph "Duis a luctus purus. Sed euismod nisi non nulla aliquam tempor at sed augue. Nullam nulla turpis, consequat sit amet ultrices ut, mollis vel massa."
                BlockToken.UnorderedListItem "* Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae;"
                BlockToken.UnorderedListItem "* Ut sagittis augue sapien. Aliquam at justo facilisis, varius diam vel, fringilla ligula."
                BlockToken.UnorderedListItem "* Pellentesque porttitor facilisis eros nec rhoncus."
                BlockToken.Empty
                BlockToken.OrderListItem "1. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae;"
                BlockToken.OrderListItem "2. Ut sagittis augue sapien. Aliquam at justo facilisis, varius diam vel, fringilla ligula."
                BlockToken.OrderListItem "3. Pellentesque porttitor facilisis eros nec rhoncus."
                BlockToken.Empty
                BlockToken.CodeBlock "let message = \"Hello, World!\""
            ]
        
        let actual = parseBlocks input    
            
        Assert.AreEqual(expected, actual)
        

