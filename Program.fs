open rtmpoop.ChunkStream

printfn "Starting up"
let appstate = createAppState 1935
printfn "Created app state"

match run appstate with
| Ok(_) -> printfn "Impossible"
| Error e -> printfn "%s" e