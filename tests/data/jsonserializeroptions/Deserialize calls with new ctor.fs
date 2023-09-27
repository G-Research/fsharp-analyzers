module M

open System.Text.Json

let f (json: string) (jsonStream: System.IO.Stream) =
    let _ = JsonSerializer.Deserialize<string>(json, new JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsync<string>(jsonStream, new JsonSerializerOptions ())
    let _ = JsonSerializer.DeserializeAsyncEnumerable<string>(jsonStream, new JsonSerializerOptions ())
    ()