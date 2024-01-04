module CST

let v (c:'c) : unit =
    (c :> System.IDisposable).Dispose()
