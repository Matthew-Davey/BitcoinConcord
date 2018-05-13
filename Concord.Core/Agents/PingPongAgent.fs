module PingPongAgent

open MessageTypes
open Envelope

let create () =
  MailboxProcessor.Start (fun inbox ->
    let rec messageLoop () = async {
      let! (peer, message:PingPong) = inbox.Receive()

      printfn "%A" message

      PingPong.create (message.nonce)
      |> PingPong.toBuffer
      |> Envelope.wrap 3652501241u (Command "pong")
      |> Peer.send <| peer
      |> ignore

      return! messageLoop()
    }
    messageLoop()
  )
