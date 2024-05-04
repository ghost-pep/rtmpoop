module rtmpoop.ChunkStream

type Version = Version of uint8

let rtmpoopVersion = Version 3uy


let createVersion (b: uint8) =
    match b with
    | x when x <= 2uy -> Error "Deprecated version"
    | x when x >= 32uy -> Error "Version not allowed to disambiguate from text-based protocols"
    | x -> Ok(Version x)


type Message0 = { version: Version }

let createMessage0 = { version = rtmpoopVersion }

type Time = Time of uint32

let epochStart = Time 0u

type Message1 =
    {
      /// Time used as the epoch for all future timestamps
      time: Time
      /// Any random data of length 1528 bytes
      random_data: uint8 array }

let randomDataLen = 1528

let createMessage1 =
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
      random_echo: uint8 array }

let createMessage2 (cur: Time) (m1: Message1) =
    { time = m1.time
      time2 = cur
      random_echo = m1.random_data }

type HandshakeState =
    | Uninitialized
    | VersionSent
    | AckSent
    | Done

type Message =
    | M0 of Message0
    | M1 of Message1
    | M2 of Message2

let sendM0M1 () = Ok()

let receiveM0 (msg: Message0) =
    match msg.version with
    | v when v <= rtmpoopVersion -> sendM0M1 ()
    | _ -> Error "Unsupported version detected"

let receive (state: HandshakeState) (msg: Message) =
    match state with
    | Uninitialized ->
        match msg with
        | M0 m -> receiveM0 m
        | _ -> Error "Uh oh stinky this is a bad message you sent me"
    | VersionSent -> Error "not impl"
    | AckSent -> Error "not impl"
    | Done -> Error "not impl"
