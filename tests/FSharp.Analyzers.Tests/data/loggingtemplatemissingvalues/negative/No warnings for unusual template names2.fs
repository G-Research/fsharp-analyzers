module M

    open Microsoft.Extensions.Logging

    let testlog () =
        use factory = LoggerFactory.Create(fun b -> b.AddConsole() |> ignore)
        let logger: ILogger = factory.CreateLogger("Program")
        let someString = "someString"
        let someOtherString = "someOtherString"

        logger.LogDebug("Blah {s_thing:l} foo {s_other_thing:l}", someString, someOtherString)
