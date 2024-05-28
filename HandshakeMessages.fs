module HandshakeMessages

open Time

type Version = byte
type Message0 = { version: Version }
type Message1 = { time: Time; random_data: byte array }

type Message2 =
    { peer_time: Time
      local_time: Time
      random_echo: uint8 array }

let rtmpoopVersion = 3uy
let message0Size = 1
let randomDataSize = 1528

let parseVersion (b: byte) =
    match b with
    | x when x <= 2uy -> Error "Deprecated version"
    | x when x >= 32uy -> Error "Version not allowed to disambiguate from text-based protocols"
    | x -> Ok(x)

let createMessage0 () = { version = rtmpoopVersion }

let parseMessage0 (raw: byte array) =
    match raw.Length with
    | l when l = message0Size -> parseVersion raw[0] |> Result.bind (fun ver -> Ok { version = ver })
    | _ -> Error "Incorrect message 0 byte length"

let serializeM0 (m: Message0) = [| byte m.version |]

let message1Size = timeSize + timeSize + randomDataSize

let createMessage1 () =
    let rand = System.Random() in
    let rand_data = [| for _ in 0 .. randomDataSize - 1 -> byte (rand.Next()) |]

    { time = epochStart
      random_data = rand_data }

let parseMessage1 (raw: byte array) =
    match raw.Length with
    | l when l = message1Size ->
        (Ok
            { time = System.BitConverter.ToUInt32 raw[0..3]
              random_data = raw[8 .. 8 + randomDataSize - 1] })
    | _ -> Error "Incorrect message 1 byte length"

let serializeM1 (m: Message1) =
    let time = System.BitConverter.GetBytes m.time
    let zero = Array.append time (System.BitConverter.GetBytes 0u)
    Array.append zero m.random_data

let message2Size = timeSize + timeSize + randomDataSize

let createMessage2 (cur: Time) (m1: Message1) =
    { peer_time = m1.time
      local_time = cur
      random_echo = m1.random_data }

let parseMessage2 (raw: byte array) =
    match raw.Length with
    | l when l = message2Size ->
        (Ok
            { peer_time = System.BitConverter.ToUInt32 raw[0..3]
              local_time = System.BitConverter.ToUInt32 raw[4..7]
              random_echo = raw[8 .. 8 + randomDataSize - 1] })
    | _ -> Error "Incorrect message 2 byte length"

let serializeM2 (m: Message2) =
    let peer = System.BitConverter.GetBytes m.peer_time
    let local = Array.append peer (System.BitConverter.GetBytes m.local_time)
    Array.append local m.random_echo
