module RTMPMessages

open Time

type StreamId = uint32
type BufferSize = uint32
type TransactionId = uint32

type UserControlEvent =
    | StreamBegin of StreamId
    | StreamEOF of StreamId
    | StreamDry of StreamId
    | SetBufferLength of StreamId * BufferSize
    | StreamIsRecorded of StreamId
    | PingRequest of Time
    | PingResponse of Time

[<System.Flags>]
type SupportedAudioCodecs =
    | None = 0x0001
    | ADPCM = 0x0002
    | MP3 = 0x0004
    | Intel = 0x0008
    | UNUSED = 0x0010
    | Nelly8 = 0x0020
    | Nelly = 0x0040
    | G711U = 0x0080
    | Nelly16 = 0x0100
    | AdvancedAudioCoding = 0x0200
    | Speex = 0x0800
    | All = 0x0FFF

[<System.Flags>]
type SupportedVideoCodecs =
    | Unused = 0x0001
    | Jpeg = 0x0002
    | Sorenson = 0x0004
    | Homebrew = 0x0008
    | VP6 = 0x0010
    | VP6Alpha = 0x0020
    | HomebrewV = 0x0040
    | H264 = 0x0080
    | All = 0x00FF

[<System.Flags>]
type SupportedVideoFunctions =
    | ClientSeek = 0x0001

type AMFEncodingMethod =
    | AMF0 = 0
    | AMF3 = 3

type ConnectData =
    { app: string
      flashver: string
      swfUrl: string
      tcUrl: string
      fpad: bool
      audioCodecs: SupportedAudioCodecs
      videoCodecs: SupportedVideoCodecs
      videoFunction: SupportedVideoFunctions
      pageUrl: string
      objectEncoding: AMFEncodingMethod }

type ConnectResponseData = { properties: obj; info: obj }

type CallData =
    { command_object: obj option
      optional_args: obj option }

type CallResponseData =
    { command_object: obj option
      response: obj }

type CreateStreamData = { command_object: obj option }

type CreateStreamResponseData =
    { command_object: obj option
      stream: Result<StreamId, obj> }

type OnStatusLevel =
    | Warning
    | Status
    | Error

type OnStatusData =
    { level: OnStatusLevel
      code: string
      description: string }

type PlayData =
    { stream_name: string
      start: int option
      duration: int option
      reset: bool option }

type Play2Data =
    // TODO: formalize these from the ActionScript 3 Language Reference under the NetStreamPlayOptions object
    { parameters: obj }

type DeleteStreamData = { stream: StreamId }

type PublishingType =
    | Live
    | Record
    | Append

type PublishData =
    { publishing_name: string
      publishing_type: PublishingType }

type PauseData = { should_pause: bool; ms: Time }

type Command =
    // NetConnection Commands
    | Connect of ConnectData
    | ConnectResponse of ConnectResponseData
    | Call of CallData
    | CallResponse of CallResponseData
    | CreateStream of CreateStreamData
    | CreateStreamResponse of CreateStreamResponseData
    // NetStream Commands
    | Play of PlayData
    | Play2 of Play2Data
    | DeleteStream of DeleteStreamData
    | ReceiveAudio of bool
    | ReceiveVideo of bool
    | Publish of PublishData
    | Seek of Time
    | Pause of PauseData
    | OnStatus of OnStatusData

type FlashEventType =
    | Use
    | Release
    | RequestChange
    | Change
    | Success
    | SendMessage
    | Status
    | Clear
    | Remove
    | RequestRemove
    | UseSuccess

type FlashEvent =
    { ev_type: FlashEventType
      len: uint32
      data: byte array }

type FlashObject =
    { name: string
      version: int
      flags: byte
      events: FlashEvent seq }

type Message =
    | UserControl of UserControlEvent
    | AMF0Command of Command
    | AMF3Command of Command
    | AMF0Data
    | AMF3Data
    | AMF0SharedObject of FlashObject
    | AMF3SharedObject of FlashObject
    | Audio
    | Video
    | Aggregate

type RTMPMessage = StreamId * Message

let serializeMessageType ((_, msg): RTMPMessage) =
    match msg with
    | UserControl _ -> 4uy
    | AMF0Command _ -> 20uy
    | AMF3Command _ -> 17uy
    | AMF0Data -> 18uy
    | AMF3Data -> 15uy
    | AMF0SharedObject _ -> 19uy
    | AMF3SharedObject _ -> 16uy
    | Audio -> 8uy
    | Video -> 9uy
    | Aggregate -> 22uy

let serializeMessage (msg: RTMPMessage) =
    raise (new System.NotImplementedException())
