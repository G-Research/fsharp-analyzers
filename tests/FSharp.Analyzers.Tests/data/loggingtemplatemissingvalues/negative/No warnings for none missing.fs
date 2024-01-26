module M

    open Microsoft.Extensions.Logging

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")

        logger.Log(LogLevel.Information, "xxx {one} yyy {two}", 23, 42)
