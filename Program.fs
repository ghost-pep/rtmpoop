open ChunkStream

printfn "Starting up"

let appstate =
    createAppState (System.Net.IPAddress([| 127uy; 0uy; 0uy; 1uy |])) 1935

printfn "Created app state"

match run appstate with
| Ok(_) -> printfn "Impossible"
| Error e -> printfn "%s" e
