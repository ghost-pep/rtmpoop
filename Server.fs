module Server

open System.Net.Sockets
open ChunkMessages
open Network

type ServerState = { hi: string }

let createServerState () = { hi = "hi" }

let receive_chunk (stream: NetworkStream) (state: ServerState) =
    printfn "Trying to read single byte"
    let fst = readN stream 1 |> Array.head |> byte
    printfn "Read byte"
    let fmt = parseFmt fst
    printfn "Got fmt %A" fmt
    let cs_id_raw = parseChunkStreamId fst

    let cs_id: ChunkMessages.StreamId =
        let sub_64 = (fun c -> c - 64u)

        match cs_id_raw with
        | 0uy -> readN stream 1 |> Array.head |> uint32 |> sub_64
        | 1uy -> readN stream 2 |> System.BitConverter.ToUInt32 |> sub_64
        | x -> cs_id_raw |> uint32

    printfn "Got stream id %A" cs_id

    Ok(createServerState ())
