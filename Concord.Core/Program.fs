open System
open System.Net
open System.Threading.Tasks
open Buffer
open Envelope
open Peer

let inline private (>>=) m fn = Result.bind fn m
let inline private (<!>) m fn = Result.map fn m

[<EntryPoint>]
let main _ =
  let inventoryAgent      = InventoryAgent.create()
  let pingPongAgent       = PingPongAgent.create()
  let unknownMessageAgent = UnknownMessageAgent.create()

  let emptyNetworkAddress = NetworkAddress.create None 1UL (IPEndPoint(IPAddress.Parse("0.0.0.0"), 0))
  let version = 
    Version.create 70015 1UL DateTimeOffset.UtcNow emptyNetworkAddress emptyNetworkAddress 4264543111543658341UL "/satoshi:0.14.0/" 0 true
    |> Version.toBuffer
    |> Envelope.wrap 3652501241u (Command "version")
  let verack =
    Buffer.empty
    |> Envelope.wrap 3652501241u (Command "verack")

  let dispatch (peer:Peer) ({command = command; payload = payload} as envelope) =
    match command with
    | Command "inv" ->
        match Inv.read (Buffer payload) with
        | Some (message, _) -> inventoryAgent.Post (peer, message)
        | None -> ()
    | Command "ping" ->
        match PingPong.read (Buffer payload) with
        | Some (message, _) -> pingPongAgent.Post (peer, message)
        | None -> ()
    | Command "tx" ->
        match Tx.read (Buffer payload) with
        | Some (message, _) -> printfn "%A" message
        | None -> ()
    | _ -> unknownMessageAgent.Post (peer, envelope)

  Peer.connect "seed.bitcoin.sipa.be" 8333us
  >>= Peer.send version
  >>= Peer.send verack
  <!> fun peer ->
        let rec poll peer =
          Task.Delay(1000) |> Async.AwaitTask |> Async.RunSynchronously
          match Peer.poll peer with
          | Result.Ok (peer, messages) ->
            printfn "%A" messages
            List.iter (dispatch peer) messages
            poll peer
          | Error _ -> Error peer
        poll peer
  |> function
     | Ok _ -> 0
     | Error _ -> -1
