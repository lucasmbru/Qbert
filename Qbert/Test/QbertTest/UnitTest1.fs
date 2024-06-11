namespace QbertTest

open NUnit.Framework

[<TestFixture>]
type MyTests() =

    [<Test>]
    member this.``Test Example`` () =
        Assert.AreEqual(1, 1)
