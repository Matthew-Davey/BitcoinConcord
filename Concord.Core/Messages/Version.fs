module Version

open System
open MessageTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let read buffer =
  Buffer.readInt32LE buffer        >>= (fun (protocolVersion, buffer) ->
  Buffer.readUint64LE buffer       >>= (fun (services, buffer) ->
  Buffer.readInt64LE buffer        >>= (fun (timestamp, buffer) ->
  NetworkAddress.read false buffer >>= (fun (receivingAddress, buffer) ->
  NetworkAddress.read false buffer >>= (fun (sendingAddress, buffer) ->
  Buffer.readUint64LE buffer       >>= (fun (nonce, buffer) ->
  VarString.read buffer            >>= (fun (userAgent, buffer) ->
  Buffer.readInt32LE buffer        >>= (fun (startHeight, buffer) ->
  Buffer.readBool buffer           <!> (fun (relay, remaining) ->
    let message =
      {
        protocolVersion  = protocolVersion
        services         = services
        timestamp        = timestamp
        receivingAddress = receivingAddress
        sendingAddress   = sendingAddress
        nonce            = nonce
        userAgent        = userAgent
        startHeight      = startHeight
        relay            = relay
      }
    (message, remaining)
  )))))))))

let create protocolVersion services (timestamp:DateTimeOffset) receivingAddress sendingAddress nonce userAgent startHeight relay =
  {
    protocolVersion  = protocolVersion
    services         = services
    timestamp        = timestamp.ToUnixTimeSeconds()
    receivingAddress = receivingAddress
    sendingAddress   = sendingAddress
    nonce            = nonce
    userAgent        = userAgent
    startHeight      = startHeight
    relay            = relay
  }

let write message = 
  Buffer.writeInt32LE message.protocolVersion
  >> Buffer.writeUint64LE message.services
  >> Buffer.writeInt64LE message.timestamp
  >> NetworkAddress.write message.receivingAddress
  >> NetworkAddress.write message.sendingAddress
  >> Buffer.writeUint64LE message.nonce
  >> VarString.write message.userAgent
  >> Buffer.writeInt32LE message.startHeight
  >> Buffer.writeBool message.relay

let toBuffer message = write message (Buffer.empty)