module Server

open System.Net.Sockets
open ChunkMessages
open Network

type ServerState = { prev_chunk: Chunk option }

let createServerState () = { prev_chunk = None }

let receive_chunk (stream: NetworkStream) (state: ServerState) =
    let fst = readN stream 1 |> Array.head |> byte
    let fmt = parseFmt fst
    printfn "Got fmt %A" fmt

    let cs_id: ChunkMessages.StreamId =
        match parseChunkStreamId fst with
        | 0u -> readN stream 1 |> parseChunkStreamId1
        | 1u -> readN stream 2 |> parseChunkStreamId2
        | x -> x

    printfn "Got stream id %A" cs_id

    let Ok (unparsed_chunk, msg_type, msg_stream_id) =
        match fmt with
        | HeaderType.Type0 -> readN stream type0Size |> parseType0 cs_id
        | HeaderType.Type1 -> readN stream type1Size |> parseType1 state.prev_chunk cs_id
        | HeaderType.Type2 -> readN stream Type2Size |> parseType2 state.prev_chunk cs_id
        | HeaderType.Type3 -> parseType3 state.prev_chunk cs_id
        | _ -> Error "Impossible Message HeaderType"

    Error "Not implemented yet"
