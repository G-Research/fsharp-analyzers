module M

open System.Text.Json

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Serialize<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeAsync<string>(jsonStream, json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToDocument<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToElement<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToNode<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.SerializeToUtf8Bytes<string>(json, new JsonSerializerOptions ())
    ()
