module Handshake

open System.Net.Sockets
open HandshakeMessages
open Time

type HandshakeState =
    | Uninitialized
    | VersionSent
    | AckSent
    | Done

let sendM0M1 (stream: NetworkStream) =
    createMessage0 () |> serializeM0 |> stream.Write
    createMessage1 () |> serializeM1 |> stream.Write
    Ok()

let sendM2 (stream: NetworkStream) (msg: Message1) =
    let cur_time = calculateCurTime System.DateTime.Now
    createMessage2 cur_time msg |> serializeM2 |> stream.Write
    Ok()

let handshake_next (state: HandshakeState) (stream: NetworkStream) =
    match state with
    | Uninitialized ->
        let received_m0_bytes = Array.zeroCreate<byte> message0Size
        stream.ReadExactly(received_m0_bytes, 0, message0Size)
        printfn "Read %i bytes for m0" received_m0_bytes.Length

        received_m0_bytes
        |> parseMessage0
        |> Result.bind (fun _ ->
            sendM0M1 stream |> ignore
            Ok VersionSent)
    | VersionSent ->
        let readbuf = Array.zeroCreate<byte> message1Size
        stream.ReadExactly(readbuf, 0, message1Size)

        readbuf
        |> parseMessage1
        |> Result.bind (fun m1 ->
            sendM2 stream m1 |> ignore
            Ok AckSent)
    | AckSent ->
        let readbuf = Array.zeroCreate<byte> message2Size
        stream.ReadExactly(readbuf, 0, message2Size)

        readbuf |> parseMessage2 |> Result.bind (fun m2 -> Ok Done)
    | Done -> Ok Done
