open ChunkStream
open Network

printfn "Creating listener"

let listener =
    createListener (System.Net.IPAddress([| 127uy; 0uy; 0uy; 1uy |])) 1935

printfn "Listening..."

while true do
    let conn = acceptConnection listener
    printfn "Accepted connection from %A" conn.client

    match run conn with
    | Ok(_) -> printfn "Impossible"
    | Error e -> printfn "%s" e
