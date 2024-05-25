// TODO: flatten version sent state to get around non-buffered send API

module rtmpoop.ChunkStream

open System.Net.Sockets

type Version = uint8

let unimplemented() = raise (new System.NotImplementedException())

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
      socket: Socket 
      rest: byte array option}

let createSocket port =
    let s = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp)
    s.Bind(new System.Net.IPEndPoint(System.Net.IPAddress.Loopback, port))
    s.Listen(10)
    s

let createAppState (port: int) =
    { handshake_state = Uninitialized
      socket = createSocket port
      rest = None}

type Message =
    | M0 of Message0
    | M1 of Message1
    | M2 of Message2

let send (socket: Socket) (b: byte array) =
    let mutable sent = 0
    let len = b.Length

    while sent < len do
        let tosend = b[ sent .. len - 1 ]
        let n = socket.Send tosend
        printfn "Sent %i" n
        sent <- sent + n


let serializeM0 (m: Message0) = [| byte m.version |]

let serializeM1 (m: Message1) =
    System.BitConverter.GetBytes m.time |> Array.append m.random_data
    
let serializeM2 (m: Message2) =
    System.BitConverter.GetBytes m.time
    |> Array.append (System.BitConverter.GetBytes m.time2)
    |> Array.append m.random_echo

let sendM0 () =
    let m0 = createMessage0 ()
    serializeM0 m0

let sendM1 () =
    let m = createMessage1 ()
    serializeM1 m

let receiveM0 (socket: Socket) (msg: Message0) =
    match msg.version with
    | v when v <= rtmpoopVersion ->
        Array.append (sendM0 ()) (sendM1 ())
        |> send socket
        printfn "Sent M0 M1"
        Ok()
    | _ -> Error "Unsupported version detected"
    
let receiveM1 (socket: Socket) (msg: Message1) =
    createMessage2 1u msg
    |> serializeM2
    |> send socket
    printfn "Sent M2"
    Ok ()
    

let recv (state: AppState) =
    match state.rest with
    | Some (bytes) -> 
        printfn "Replaying %i previously unparsed bytes" bytes.Length
        bytes
    | None -> 
        let mutable buf = [||]
        let mutable total = 0
        let mutable keep_reading = true

        while keep_reading do
            let read_buf: byte array = Array.zeroCreate 1024
            let n = state.socket.Receive read_buf
            printfn "Read %i bytes" n

            if n = 0 then
                keep_reading <- false
            else
                buf <- Array.append buf read_buf
                total <- total + n

        printfn "Finished reading %i bytes" total
        buf
    
let parseMessage0 (msg_bytes: byte array) =
    if msg_bytes.Length <= 1 then
        Error "Not enough bytes for a parsed M0"
    else
        createVersion msg_bytes[0] |> Result.bind (fun ver -> Ok ({ version = ver }, msg_bytes[1..]))
        
let parseMessage1 (msg_bytes: byte array) : Result<Message1 * byte array, string>= 
    if msg_bytes.Length < 1528 + 4 then Error "Not enough bytes for a parsed M1"
    else(
        printfn "Parsed M1"
        Ok ({time = System.BitConverter.ToUInt32 msg_bytes[0..3] 
             // we skip an extra word here because the spec has zeros in time2 slot (which I didn't represent in data model)
             random_data = msg_bytes[8..8 + 1528 - 1]},
            msg_bytes[4 + 1528..])
    )
        
let processRestOfBytes b =
    match b with
    | [||] -> None
    | x -> Some x
        
let receive (state: AppState) =
    match state.handshake_state with
    | Uninitialized ->
        let handler = state.socket.Accept()
        let initialized_state = {state with socket = handler}
        let received_bytes = recv initialized_state
        received_bytes 
        |> parseMessage0
        |> Result.bind (fun (m0, rest) -> 
                receiveM0 handler m0
                |> Result.bind (fun _ -> 
                    Ok(rest))
            )
        |> Result.bind (fun rest ->
            Ok {    handshake_state = VersionSent
                    socket = handler
                    rest = processRestOfBytes rest })
    | VersionSent ->
        recv state
        |> parseMessage1
        |> Result.bind (fun (m1, rest) -> 
                printfn "M1 is %A" m1
                receiveM1 state.socket m1
                |> Result.bind (fun _ -> Ok (rest))
            )
        |> Result.bind (fun rest ->
            Ok {state with    
                    handshake_state = VersionSent
                    rest = processRestOfBytes rest })
    | AckSent -> Error "not impl"
    | Done -> Error "not impl"


let rec run (state: AppState) =
    printfn "Running with state %A" state
    receive state
    |> Result.bind run