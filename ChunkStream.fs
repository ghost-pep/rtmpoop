module rtmpoop.ChunkStream

open System.Net.Sockets

type Version = uint8

let rtmpoopVersion = 3uy


let createVersion (b: uint8) =
    match b with
    | x when x <= 2uy -> Error "Deprecated version"
    | x when x >= 32uy -> Error "Version not allowed to disambiguate from text-based protocols"
    | x -> Ok(x)


type Message0 = { version: Version }

let createMessage0 () = { version = rtmpoopVersion }

type Time = uint32

let epochStart = 0u

type Message1 =
    {
        /// Time used as the epoch for all future timestamps
        time: Time
        /// Any random data of length 1528 bytes
        random_data: uint8 array
    }

let randomDataLen = 1528

let createMessage1 () =
    let rand = System.Random() in

    { time = epochStart
      random_data = [| for _ in 0 .. randomDataLen - 1 -> uint8 (rand.Next()) |] }

type Message2 =
    {
        /// Peer's timestamp received from Message1
        time: Time
        /// Local timestamp when Message1 was received
        time2: Time
        /// Random data field from Message1
        random_echo: uint8 array
    }

let createMessage2 (cur: Time) (m1: Message1) =
    { time = m1.time
      time2 = cur
      random_echo = m1.random_data }

type HandshakeState =
    | Uninitialized
    | VersionSent
    | AckSent
    | Done

type AppState =
    { handshake_state: HandshakeState
      socket: Socket }

let createSocket () =
    let s = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    s.Bind(new System.Net.IPEndPoint(System.Net.IPAddress.Loopback, 1935))
    s.Listen(10)
    s

let createAppState () =
    { handshake_state = Uninitialized
      socket = createSocket () }

type Message =
    | M0 of Message0
    | M1 of Message1
    | M2 of Message2

let send (socket: Socket) (b: byte array) =
    let mutable sent = 0
    let len = b.Length

    while sent < len do
        let tosend = b[sent .. len - 1]
        let n = socket.Send tosend
        sent <- sent + n


let serializeM0 (m: Message0) = [| byte m.version |]

let serializeM1 (m: Message1) =
    System.BitConverter.GetBytes m.time |> Array.append m.random_data

let sendM0 (socket: Socket) =
    let m0 = createMessage0 ()
    send socket (serializeM0 m0)

let sendM1 (socket: Socket) =
    let m = createMessage1 ()
    send socket (serializeM1 m)

let receiveM0 (socket: Socket) (msg: Message0) =
    match msg.version with
    | v when v <= rtmpoopVersion ->
        socket |> sendM0

        socket |> sendM1

        Ok()
    | _ -> Error "Unsupported version detected"

let recv (socket: Socket) =
    let mutable buf = [||]
    let mutable total = 0
    let mutable keep_reading = true

    while keep_reading do
        let read_buf: byte array = Array.zeroCreate 1024
        let n = socket.Receive read_buf

        if n = 0 then
            keep_reading <- false
        else
            buf <- Array.append buf read_buf
            total <- total + n

    (buf, total)

let parseMessage0 (msg_bytes: byte array, total: int) =
    if total <= 1 then
        Error "Not enough bytes for a parsed M0"
    else
        createVersion msg_bytes[0] |> Result.bind (fun ver -> Ok { version = ver })

let receive (state: AppState) =
    match state.handshake_state with
    | Uninitialized ->
        let handler = state.socket.Accept()

        recv handler
        |> parseMessage0
        |> Result.bind (receiveM0 handler)
        |> Result.bind (fun _ ->
            Ok
                { state with
                    handshake_state = VersionSent })
    | VersionSent -> Error "not impl"
    | AckSent -> Error "not impl"
    | Done -> Error "not impl"
