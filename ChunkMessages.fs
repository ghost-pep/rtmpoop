module ChunkMessages

open Time
open RTMPMessages

type StreamId = uint32
type SequenceNumber = uint32

type HeaderType =
    | Type0 = 0uy
    | Type1 = 1uy
    | Type2 = 2uy
    | Type3 = 3uy

type BandwidthLimitType =
    | Hard
    | Soft
    | Dynamic

type ProtocolControlMessage =
    | SetChunkSize of uint32
    | Abort of StreamId
    | Ack of SequenceNumber
    | WindowAckSize of uint32
    | SetPeerBandwidth of uint32 * BandwidthLimitType

type MessageType =
    | RMTP of RTMPMessage
    | ProtocolControl of ProtocolControlMessage

type Message =
    { hdr_type: HeaderType
      ts: Time
      msg_len: byte
      msg: MessageType }

type Chunk = { stream: StreamId; msg: Message }


let serializeBasicHeader (c: Chunk) =
    let fmt = byte c.msg.hdr_type

    match c.stream with
    | x when (2u <= x) && (x < 64u) -> Ok [| (fmt <<< 6) &&& (byte c.stream) |]
    | x when (64u <= x) && (x < 320u) -> Ok [| (fmt <<< 6) &&& 0uy; byte (c.stream - 64u) |]
    | x when (320u <= x) && (x < 65600u) ->
        Ok [| (fmt <<< 6) &&& 1uy; byte ((c.stream - 64u) <<< 8); byte (c.stream - 64u) |]
    | _ -> Result.Error "Invalid chunk stream id"


let serializeMessageHeader msg =
    raise (new System.NotImplementedException())

let serializeExtendedTS c =
    raise (new System.NotImplementedException())

let serializeChunkData msg =
    raise (new System.NotImplementedException())

let combineOn f mx my =
    mx |> Result.bind (fun x -> my |> Result.bind (fun y -> Ok(f x y)))

let serializeChunk (c: Chunk) =
    serializeBasicHeader c
    |> combineOn Array.append (serializeMessageHeader c.msg)
    |> combineOn Array.append (serializeExtendedTS c)
    |> combineOn Array.append (serializeChunkData c.msg)
