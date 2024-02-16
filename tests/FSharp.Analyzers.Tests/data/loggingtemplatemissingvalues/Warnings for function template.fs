module M

    open Microsoft.Extensions.Logging

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")
        let timeWasteString = "time waste"

        logger.LogWarning(id "Foo {Wasted}\n{Result}", timeWasteString)
