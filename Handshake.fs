module Handshake

open System.Net.Sockets
open HandshakeMessages
open Time

type HandshakeState =
    | Uninitialized
    | VersionSent of byte array
    | AckSent of byte array
    | Done

let sendM0M1 (stream: NetworkStream) =
    let m0 = createMessage0 () |> serializeM0
    let m1 = createMessage1 ()
    Array.append m0 (m1 |> serializeM1) |> stream.Write
    Ok m1.random_data

let sendM2 (stream: NetworkStream) (msg: Message1) =
    let cur_time = calculateCurTime System.DateTime.Now
    createMessage2 cur_time msg |> serializeM2 |> stream.Write
    Ok()

let handshake_next (state: HandshakeState) (stream: NetworkStream) =
    match state with
    | Uninitialized ->
        let received_m0_bytes = Array.zeroCreate<byte> message0Size
        stream.ReadExactly(received_m0_bytes, 0, message0Size)

        received_m0_bytes
        |> parseMessage0
        |> Result.bind (fun _ -> sendM0M1 stream)
        |> Result.bind (fun challenge -> Ok(VersionSent challenge))
    | VersionSent challenge ->
        let readbuf = Array.zeroCreate<byte> message1Size
        stream.ReadExactly(readbuf, 0, message1Size)

        readbuf
        |> parseMessage1
        |> Result.bind (fun m1 ->
            sendM2 stream m1 |> ignore
            Ok(AckSent challenge))
    | AckSent challenge ->
        let readbuf = Array.zeroCreate<byte> message2Size
        stream.ReadExactly(readbuf, 0, message2Size)

        readbuf
        |> parseMessage2
        |> Result.bind (fun m2 ->
            if challenge = m2.random_echo then
                Ok Done
            else
                Error "Failed to validate challenge")
    | Done -> Ok Done
