module TaskWaitMethods

open System.Threading.Tasks

let testTaskWait () =
    let t = Task.Run(fun () -> 42)
    t.Wait()
    t.Result

let testTaskWaitWithTimeout () =
    let t = Task.Delay(100)
    t.Wait(1000)

let testTaskWaitAll () =
    let t1 = Task.Run(fun () -> 1)
    let t2 = Task.Run(fun () -> 2)
    Task.WaitAll(t1, t2)

let testTaskWaitAny () =
    let t1 = Task.Delay(100)
    let t2 = Task.Delay(200)
    let index = Task.WaitAny(t1, t2)
    index