module ChunkStream

open System.Net.Sockets
open Messages
open Time
open Network

type HandshakeState =
    | Uninitialized
    | VersionSent
    | AckSent
    | Done

type AppState =
    { handshake_state: HandshakeState
      listener: TcpListener
      epoch_start: System.DateTime
      stream: NetworkStream option }



let createAppState ip port =
    { handshake_state = Uninitialized
      listener = createListener ip port
      stream = None
      epoch_start = System.DateTime.Now }

let sendM0M1 (stream: NetworkStream) =
    createMessage0 () |> serializeM0 |> stream.Write
    createMessage1 () |> serializeM1 |> stream.Write
    Ok()

let sendM2 (stream: NetworkStream) (msg: Message1) =
    let cur_time = calculateCurTime System.DateTime.Now
    createMessage2 cur_time msg |> serializeM2 |> stream.Write
    Ok()

let next (state: AppState) =
    match state.handshake_state with
    | Uninitialized ->
        let client = state.listener.AcceptTcpClient()
        let stream = client.GetStream()
        let received_m0_bytes = Array.zeroCreate<byte> message0Size
        stream.ReadExactly(received_m0_bytes, 0, message0Size)
        printfn "Read %i bytes for m0" received_m0_bytes.Length

        received_m0_bytes
        |> parseMessage0
        |> Result.bind (fun _ -> sendM0M1 stream)
        |> Result.bind (fun _ ->
            Ok
                { state with
                    handshake_state = VersionSent
                    stream = Some stream })
    | VersionSent ->
        let stream = state.stream.Value
        let readbuf = Array.zeroCreate<byte> message1Size
        stream.ReadExactly(readbuf, 0, message1Size)

        readbuf
        |> parseMessage1
        |> Result.bind (fun m1 -> sendM2 stream m1)
        |> Result.bind (fun _ -> Ok { state with handshake_state = AckSent })
    | AckSent ->
        let stream = state.stream.Value
        let readbuf = Array.zeroCreate<byte> message2Size
        stream.ReadExactly(readbuf, 0, message2Size)

        readbuf
        |> parseMessage2
        |> Result.bind (fun m2 -> Ok())
        |> Result.bind (fun _ -> Ok { state with handshake_state = Done })
    | Done -> Error "not impl"


let rec run (state: AppState) =
    printfn "Running with state %A" state
    next state |> Result.bind run
