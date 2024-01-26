module M

    open Microsoft.Extensions.Logging

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")

        logger.Log(LogLevel.Information, "xxx {{esc1}} {{{{esc2}}}} {{{{{{esc3}}}}}} yyy {two}", 2)
