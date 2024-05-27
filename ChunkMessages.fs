module ChunkMessages

open Time
open RTMPMessages

type StreamId = uint32
type SequenceNumber = uint32

type HeaderType =
    | Type0
    | Type1
    | Type2
    | Type3

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
