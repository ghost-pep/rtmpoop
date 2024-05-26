module rtmpoop.ChunkStream

open System.Net.Sockets

type Version = byte

let unimplemented() = raise (new System.NotImplementedException())

let rtmpoopVersion = 3uy


let createVersion (b: uint8) =
    match b with
    | x when x <= 2uy -> Error "Deprecated version"
    | x when x >= 32uy -> Error "Version not allowed to disambiguate from text-based protocols"
    | x -> Ok(x)


type Message0 = 
    { version: Version }
    
let message0Size = 1

let createMessage0 () = 
    { version = rtmpoopVersion }

type Time = uint32

let epochStart: Time = 0u

type Message1 =
    {
        /// Time used as the epoch for all future timestamps
        time: Time
        zero_time: Time
        /// Any random data of length 1528 bytes
        random_data: uint8 array
    }

let randomDataLen = 1528
let message1Size = 4 + 4 + randomDataLen

let createMessage1 () =
    let rand = System.Random() in

    { time = epochStart
      zero_time = 0u
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
      listener : TcpListener
      epoch_start: System.DateTime
      stream: NetworkStream option}


let createListener port =
    let listener = 
        new TcpListener(
            System.Net.IPAddress.Loopback,
            port)
    listener.Start(10)
    listener

let createAppState (port: int) =
    { handshake_state = Uninitialized
      listener = createListener port
      stream = None
      epoch_start = System.DateTime.Now}

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

let receiveM0 (stream: NetworkStream) (msg: Message0) =
    match msg.version with
    | v when v <= rtmpoopVersion ->
        sendM0 () |> stream.Write
        sendM1 () |> stream.Write
        printfn "Sent M0 M1"
        Ok()
    | _ -> Error "Unsupported version detected"
    
let calculateCurTime (epoch_start: System.DateTime) =
    let cur = System.DateTime.Now
    let diff = cur.Subtract(epoch_start)
    diff.TotalMilliseconds
    |> uint32
    
let receiveM1 (stream: NetworkStream) (epoch_start: System.DateTime) (msg: Message1) =
    let cur_time = calculateCurTime epoch_start
    createMessage2 cur_time msg
    |> serializeM2
    |> stream.Write
    printfn "Sent M2"
    Ok ()

let parseMessage0 (msg_bytes: byte array) =
    createVersion msg_bytes[0] 
    |> Result.bind (fun ver ->
        Ok { version = ver } )
        
let parseMessage1 (msg_bytes: byte array) : Result<Message1, string>= 
    if msg_bytes.Length < 1528 + 4 then Error "Not enough bytes for a parsed M1"
    else(
        Ok { time = System.BitConverter.ToUInt32 msg_bytes[0..3] 
             zero_time = System.BitConverter.ToUInt32 msg_bytes[4..7]
             // we skip an extra word here because the spec has zeros in time2 slot (which I didn't represent in data model)
             random_data = msg_bytes[8..8 + randomDataLen - 1]})
        
let processRestOfBytes b =
    match b with
    | [||] -> None
    | x -> Some x
        
let receive (state: AppState) =
    match state.handshake_state with
    | Uninitialized ->
        let client = state.listener.AcceptTcpClient()
        let stream = client.GetStream()
        let received_m0_bytes = Array.zeroCreate<byte> message0Size
        stream.ReadExactly(received_m0_bytes, 0, message0Size)
        printfn "Read %i bytes for m0" received_m0_bytes.Length
        received_m0_bytes 
        |> parseMessage0
        |> Result.bind (fun m0 -> 
                receiveM0 stream m0)
        |> Result.bind (fun _ -> 
            Ok {state with 
                    handshake_state = VersionSent
                    stream = Some stream})
    | VersionSent ->
        let stream = state.stream.Value
        let readbuf = Array.zeroCreate<byte> message1Size
        stream.ReadExactly(readbuf, 0, message1Size)
        printfn "Read %i bytes for m1" readbuf.Length
        readbuf
        |> parseMessage1
        |> Result.bind (fun m1 -> 
                printfn "M1 is %A" m1
                printfn "M1 rand_data len is %i" m1.random_data.Length
                receiveM1 stream  state.epoch_start m1
            )
        |> Result.bind (fun _ ->
            Ok {state with    
                    handshake_state = AckSent})
    | AckSent -> Error "not impl"
    | Done -> Error "not impl"


let rec run (state: AppState) =
    printfn "Running with state %A" state
    receive state
    |> Result.bind run