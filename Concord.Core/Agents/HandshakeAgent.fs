module HandshakeAgent

open System
open System.Net
open MessageTypes
open Envelope

type Message =
  | Begin // Begin the handshake process
  | Version of Version // Remote peer sent a version message
  | Verack // Remote peer sent a verack message
  | Timeout // Timeout reached

type private ProcessState =
  {
    versionSent:bool
    versionReceived:Version option
    verackReceived:bool
    completed:bool
  }

type Result =
  | Success of int32
  | Failure
  | TimedOut

let private startingState = {
  versionSent = false
  versionReceived = None
  verackReceived = false
  completed = false
}

let create peer cb =
  MailboxProcessor.Start (fun inbox ->
    let rec messageLoop state = async {
      let! message = inbox.Receive()

      match message with
      | Begin ->
          let emptyNetworkAddress = NetworkAddress.create None 1UL (IPEndPoint(IPAddress.Parse("0.0.0.0"), 0))
          let version = 
            Version.create 70015 1UL DateTimeOffset.UtcNow emptyNetworkAddress emptyNetworkAddress 4264543111543658341UL "/satoshi:0.14.0/" 0 true
            |> Version.toBuffer
            |> Envelope.wrap 3652501241u (Command "version")

          match Peer.send version peer with
          | Ok _ -> return! messageLoop {state with versionSent = true}
          | Error exn ->
                cb Failure
      | Version msg ->
          return! messageLoop {state with versionReceived = Some msg}
      | Verack ->
          let verack = Envelope.wrap 3652501241u (Command "verack") Buffer.empty
          match Peer.send verack peer with
          | Ok _ ->
              return! messageLoop {state with verackReceived = true; completed = true}
          | Error exn ->
              cb Failure
      | Timeout ->
          if not state.completed then
            cb TimedOut
    }
    messageLoop startingState
  )

let startHandshake (agent:MailboxProcessor<Message>) =
  agent.Post Message.Begin
