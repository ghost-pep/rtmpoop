module Network

open System.Net.Sockets

let createListener ip port =
    let listener = new TcpListener(ip, port)
    listener.Start(10)
    listener

let getClient (listener: TcpListener) = listener.AcceptTcpClient()
let getStream (client: TcpClient) = client.GetStream()

let readN (stream: NetworkStream) (n: int) =
    let read_buf = Array.zeroCreate n
    stream.ReadExactly(read_buf, 0, n)
    read_buf
