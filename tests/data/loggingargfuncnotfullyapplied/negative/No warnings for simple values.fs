module M

    open Microsoft.Extensions.Logging

    let someString = "abc"

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")

        logger.Log(LogLevel.Information, "should show: {0} {1}", someString, "bla")
