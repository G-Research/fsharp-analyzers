module M

    open Microsoft.Extensions.Logging

    let f a b c = $"here is f with {a + b + c}"

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")

        logger.Log(LogLevel.Information, "should show: {0}", 1 + 2)
