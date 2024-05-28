module ChunkMessages

open Time
open RTMPMessages
open Utils

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
    | RTMP of RTMPMessage
    | ProtocolControl of ProtocolControlMessage
    | Unparsed

type Message =
    { hdr_type: HeaderType
      ts: Time
      msg_len: uint32
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

let max_chunk_ts = 0xFFFFFFu

let serializeTimestamp (ts_raw: Time) =
    match ts_raw with
    | ts when ts >= max_chunk_ts -> Ok(System.BitConverter.GetBytes(max_chunk_ts))
    | ts -> Ok(System.BitConverter.GetBytes(ts)[1..3])

let serializeProtocolControlMessageType (msg: ProtocolControlMessage) =
    match msg with
    | SetChunkSize _ -> 1uy
    | Abort _ -> 2uy
    | Ack _ -> 3uy
    | WindowAckSize _ -> 5uy
    | SetPeerBandwidth _ -> 6uy

let serializeMessageType (msg: MessageType) =
    match msg with
    | RTMP x -> Ok([| serializeMessageType x |])
    | ProtocolControl x -> Ok([| serializeProtocolControlMessageType x |])
    | Unparsed -> Result.Error "Cannot serialize an unparsed message"

let serializeMessageStreamId (msg: MessageType) =
    match msg with
    | RTMP(stream_id, _) -> Ok(System.BitConverter.GetBytes(stream_id))
    | ProtocolControl _ -> Ok(System.BitConverter.GetBytes(0u))
    | Unparsed -> Result.Error "Cannot serialize an unparsed message"

let serializeMessageHeader (msg: Message) (ts_raw: Time) (delta: Time option) =
    match msg.hdr_type with
    | HeaderType.Type0 ->
        serializeTimestamp ts_raw
        |> combineOn Array.append (Ok(System.BitConverter.GetBytes(msg.msg_len)[1..3]))
        |> combineOn Array.append (serializeMessageType msg.msg)
        |> combineOn Array.append (serializeMessageStreamId msg.msg)
    | HeaderType.Type1 ->
        let delta_res =
            match delta with
            | Some d -> Ok d
            | None -> Result.Error "No delta passed"

        delta_res
        |> Result.bind serializeTimestamp
        |> combineOn Array.append (Ok(System.BitConverter.GetBytes(msg.msg_len)[1..3]))
        |> combineOn Array.append (serializeMessageType msg.msg)
    | HeaderType.Type2 ->
        let delta_res =
            match delta with
            | Some d -> Ok d
            | None -> Result.Error "No delta passed"

        delta_res |> Result.bind serializeTimestamp
    | HeaderType.Type3 -> Ok [||]
    | _ -> Result.Error "Impossible Message HeaderType"



let serializeExtendedTS (msg: HeaderType) (ts: Time) (delta: Time option) =
    match msg with
    | HeaderType.Type0 ->
        match ts with
        | t when t >= max_chunk_ts -> Ok(System.BitConverter.GetBytes(ts))
        | _ -> Ok [||]
    | HeaderType.Type1
    | HeaderType.Type2 ->
        let delta_res =
            match delta with
            | Some d -> Ok d
            | None -> Result.Error "No delta passed"

        delta_res
        |> Result.bind (fun d -> Ok(uint32 d))
        |> Result.bind (fun d -> Ok(System.BitConverter.GetBytes(d)))
    | HeaderType.Type3 -> Ok [||]
    | _ -> Result.Error "Impossible Message HeaderType"

let serializeMessage (m: ProtocolControlMessage) =
    raise (new System.NotImplementedException())

let serializeChunkData (msg: Message) =
    match msg.msg with
    | RTMP m -> Ok(RTMPMessages.serializeMessage m)
    | ProtocolControl m -> Ok(serializeMessage m)
    | Unparsed -> Result.Error "Cannot serialize an unparsed message"


let serializeChunk (c: Chunk) (epoch_start: System.DateTime) (prev_chunk_at: Time option) =
    let ts = calculateCurTime epoch_start

    let delta: Time option =
        prev_chunk_at |> Option.bind (fun prev -> Some((uint32 ts) - (uint32 prev)))

    serializeBasicHeader c
    |> combineOn Array.append (serializeMessageHeader c.msg ts delta)
    |> combineOn Array.append (serializeExtendedTS c.msg.hdr_type ts delta)
    |> combineOn Array.append (serializeChunkData c.msg)

let parseFmt (data: byte) =
    LanguagePrimitives.EnumOfValue<byte, HeaderType>(data >>> 6)

let parseChunkStreamId (data: byte) = (data <<< 2) >>> 2
