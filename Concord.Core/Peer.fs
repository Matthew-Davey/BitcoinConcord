module Peer

open System
open System.Net.Sockets
open Buffer

type Peer =
  {
    id     : Guid
    socket : TcpClient
    buffer : Buffer
  }

let private try' expr =
  try Ok expr
  with | exn -> Error exn

let inline private (>>=) m fn = Result.bind fn m
let inline private (<!>) m fn = Result.map fn m

let connect (address:string) (port:uint16) =
  try
    let socket = new TcpClient()
    socket.Connect(address, int port)
    Ok socket
  with
  | exn -> Error exn
  <!> fun socket ->
    {id = Guid.NewGuid(); socket = socket; buffer = Buffer.empty}

let send envelope ({socket = socket} as peer) =
  Envelope.toBuffer envelope
  |> Buffer.write (socket.GetStream())
  <!> fun _ -> peer

let poll ({socket = socket} as peer) =
  match socket.Available with
  | 0 -> Ok (peer, [])
  | _ ->
    try' (socket.GetStream())
    >>= Buffer.read (uint16 socket.Available)
    <!> Buffer.appendBuffer peer.buffer
    <!> Envelope.readAll
    <!> fun (responses, remaining) ->
          ({peer with buffer = remaining}, responses)

let disconnect {socket = socket} = socket.Close()
