open rtmpoop.ChunkStream

printfn "Starting up"
let appstate = createAppState ()
printfn "Created app state"
let state2 = receive appstate
printfn "Received state"

match state2 with
| Ok(s) -> printfn "Received state: %A" s
| Error(e) -> printfn "Error: %s" e
