module M

open System.Text.Json

let cachedOptions = new JsonSerializerOptions ()

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Serialize<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeAsync<string>(jsonStream, json, cachedOptions)
    let _ = JsonSerializer.SerializeToDocument<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeToElement<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeToNode<string>(json, cachedOptions)
    let _ = JsonSerializer.SerializeToUtf8Bytes<string>(json, cachedOptions)
    ()
