module PingPong

open MessageTypes

let inline private (<!>) m fn = Option.map fn m

let create nonce =
  {
    nonce = nonce
  }

let read buffer =
  Buffer.readUint64LE buffer <!> (fun (nonce, remaining) ->
    let message =
      {
        nonce = nonce
      }
    (message, remaining)
  )

let write (message:PingPong) =
  Buffer.writeUint64LE message.nonce

let toBuffer message = write message Buffer.empty