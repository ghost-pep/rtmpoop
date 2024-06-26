module ChunkStream

open System.Net.Sockets
open Network
open Handshake
open Server

type State =
    | Handshake of HandshakeState
    | Receiving of ServerState

type Connection =
    { state: State
      client: TcpClient
      stream: NetworkStream
      epoch_start: System.DateTime }

let acceptConnection listener =
    let client = listener |> getClient
    let stream = client |> getStream

    { state = Handshake Uninitialized
      client = client
      stream = stream
      epoch_start = System.DateTime.Now }

let next (conn: Connection) =
    match conn.state with
    | Handshake x when x <> Done ->
        handshake_next x conn.stream
        |> Result.bind (fun n -> Ok { conn with state = Handshake n })
    | Handshake _ ->
        Ok
            { conn with
                state = Receiving(createServerState ()) }
    | Receiving x ->
        receive_chunk conn.stream x
        |> Result.bind (fun n -> Ok { conn with state = Receiving n })


let rec run (conn: Connection) =
    printfn "Running with state %A" conn
    next conn |> Result.bind run
