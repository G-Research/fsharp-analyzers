module M

    open Microsoft.Extensions.Logging

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")

        logger.LogDebug("Blah {xxx}} foo {yyy}", 23)
