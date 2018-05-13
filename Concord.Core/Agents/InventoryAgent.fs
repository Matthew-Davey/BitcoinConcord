module InventoryAgent

open PrimitiveTypes
open MessageTypes
open Envelope

let create () =
  MailboxProcessor.Start (fun inbox ->
    let rec messageLoop () = async {
      let! (peer, message) = inbox.Receive()

      printfn "%A" message

      let txObjects = List.filter (fun {objectType = ot} -> ot = 1u) message.inventory
      let txCount = List.length txObjects
                    |> uint32

      // Check for existence of tx in redis, if not there send getdata message...

      GetData.create txCount txObjects
      |> GetData.toBuffer
      |> Envelope.wrap 3652501241u (Command "getdata")
      |> Peer.send <| peer
      |> ignore

      return! messageLoop()
    }
    messageLoop()
  )
