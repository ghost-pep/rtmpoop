module Network

open System.Net.Sockets

let createListener ip port =
    let listener = new TcpListener(ip, port)
    listener.Start(10)
    listener

let getClient (listener: TcpListener) = listener.AcceptTcpClient()
let getStream (client: TcpClient) = client.GetStream()
