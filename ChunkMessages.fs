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
        |> combineOn appendOk (Ok(System.BitConverter.GetBytes(msg.msg_len)[1..3]))
        |> combineOn appendOk (serializeMessageType msg.msg)
        |> combineOn appendOk (serializeMessageStreamId msg.msg)
    | HeaderType.Type1 ->
        let delta_res =
            match delta with
            | Some d -> Ok d
            | None -> Result.Error "No delta passed"

        delta_res
        |> Result.bind serializeTimestamp
        |> combineOn appendOk (Ok(System.BitConverter.GetBytes(msg.msg_len)[1..3]))
        |> combineOn appendOk (serializeMessageType msg.msg)
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

let streamIdFrom (msg: Message) =
    match msg.msg with
    | RTMP(stream_id, _) -> Ok stream_id
    | ProtocolControl _ -> Ok 0u
    | Unparsed -> Result.Error "Cannot serialize an unparsed message"

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
    |> combineOn appendOk (serializeMessageHeader c.msg ts delta)
    |> combineOn appendOk (serializeExtendedTS c.msg.hdr_type ts delta)
    |> combineOn appendOk (serializeChunkData c.msg)

let parseFmt (data: byte) =
    LanguagePrimitives.EnumOfValue<byte, HeaderType>(data >>> 6)

let sub_64 = (fun c -> c - 64u)
let parseChunkStreamId1 (data: byte array) = data |> Array.head |> uint32 |> sub_64

let parseChunkStreamId2 (data: byte array) =
    data |> System.BitConverter.ToUInt32 |> sub_64

let parseChunkStreamId (data: byte) = (data <<< 2) >>> 2 |> uint32

let type0Size = 11
let type1Size = 7
let Type2Size = 3

let parseType0 (stream: StreamId) (data: byte array) =
    let ts: Time = System.BitConverter.ToUInt32(data[0..2])
    let msg_len = System.BitConverter.ToUInt32(data[3..5])
    let msg_type = data[6]
    let msg_stream_id = System.BitConverter.ToUInt32(data[7..10])

    Ok(
        { stream = stream
          msg =
            { hdr_type = HeaderType.Type0
              ts = ts
              msg_len = msg_len
              msg = Unparsed } },
        msg_type,
        msg_stream_id
    )

let parseType1 (prev_chunk: Chunk option) (stream: StreamId) (data: byte array) =
    let delta = System.BitConverter.ToUInt32(data[0..2])
    let msg_len = System.BitConverter.ToUInt32(data[3..5])
    let msg_type = data[6]

    let msg_stream_id_res =
        match prev_chunk |> Option.map (fun c -> streamIdFrom c.msg) with
        | Some(x) -> x
        | None -> Result.Error "No previous chunk to calculate stream id from"

    let ts_res =
        match prev_chunk with
        | Some c -> Ok(c.msg.ts + delta)
        | None -> Result.Error "No previous chunk to calculate timestamp from"

    ts_res
    |> combineOn (fun ts msg_stream_id -> Ok(ts, msg_stream_id)) msg_stream_id_res
    |> Result.bind (fun (msg_stream_id, ts) ->
        Ok(
            { stream = stream
              msg =
                { hdr_type = HeaderType.Type1
                  ts = ts
                  msg_len = msg_len
                  msg = Unparsed } },
            msg_type,
            msg_stream_id
        ))

let parseType2 (prev_chunk: Chunk option) (stream: StreamId) (data: byte array) =
    let prev_chunk_res =
        match prev_chunk with
        | Some c -> Ok c
        | None -> Result.Error "No previous chunk to calculate from"

    let delta = System.BitConverter.ToUInt32(data[0..2])

    let ts_res = prev_chunk_res |> Result.bind (fun c -> Ok(c.msg.ts + delta))
    let msg_len_res = prev_chunk_res |> Result.bind (fun c -> Ok(c.msg.msg_len))

    let msg_type_res =
        prev_chunk_res
        |> Result.bind (fun c -> c.msg.msg |> serializeMessageType |> Result.bind (fun b -> Ok(Array.head b)))


    // TODO: This feels wrong... is there a better way to get some nice combinator here that doesn't require me to keep tupling?
    // maybe this is a problem with not having the concept of type kinds? Idk and this is burning me out right now
    ts_res
    |> combineOn (fun msg_len ts -> Ok(ts, msg_len)) msg_len_res
    |> combineOn (fun msg_type (ts, msg_len) -> Ok(ts, msg_len, msg_type)) msg_type_res
    |> combineOn
        (fun msg_stream_id (ts, msg_len, msg_type) -> Ok(ts, msg_len, msg_type, msg_stream_id))
        (prev_chunk_res |> Result.bind (fun c -> streamIdFrom c.msg))
    |> Result.bind (fun (ts, msg_len, msg_type, msg_stream_id) ->
        Ok(
            { stream = stream
              msg =
                { hdr_type = HeaderType.Type2
                  ts = ts
                  msg_len = msg_len
                  msg = Unparsed } },
            msg_type,
            msg_stream_id
        ))

let parseType3 (prev_chunk: Chunk option) (stream: StreamId) =
    let prev_chunk_res =
        match prev_chunk with
        | Some c -> Ok c
        | None -> Result.Error "No previous chunk to calculate from"

    let ts_res = prev_chunk_res |> Result.bind (fun c -> Ok(c.msg.ts))
    let msg_len_res = prev_chunk_res |> Result.bind (fun c -> Ok(c.msg.msg_len))

    let msg_type_res =
        prev_chunk_res
        |> Result.bind (fun c -> c.msg.msg |> serializeMessageType |> Result.bind (fun b -> Ok(Array.head b)))

    let msg_stream_id_res = prev_chunk_res |> Result.bind (fun c -> streamIdFrom c.msg)

    ts_res
    |> combineOn (fun msg_len ts -> Ok(ts, msg_len)) msg_len_res
    |> combineOn (fun msg_type (ts, msg_len) -> Ok(ts, msg_len, msg_type)) msg_type_res
    |> combineOn
        (fun msg_stream_id (ts, msg_len, msg_type) -> Ok(ts, msg_len, msg_type, msg_stream_id))
        msg_stream_id_res
    |> Result.bind (fun (ts, msg_len, msg_type, msg_stream_id) ->
        Ok(
            { stream = stream
              msg =
                { hdr_type = HeaderType.Type3
                  ts = ts
                  msg_len = msg_len
                  msg = Unparsed } },
            msg_type,
            msg_stream_id
        ))
