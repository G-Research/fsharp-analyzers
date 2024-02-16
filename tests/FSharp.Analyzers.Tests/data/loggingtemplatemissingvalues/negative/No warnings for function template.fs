module M

    open Microsoft.Extensions.Logging

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")
        let timespan = System.TimeSpan()
        let timeWasteString = "time waste"
        let result = "result"

        logger.LogWarning(id "Foo {Wasted}\n{Result}", timeWasteString, result)
