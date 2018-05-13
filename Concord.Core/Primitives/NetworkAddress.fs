module NetworkAddress

open System
open System.Net
open PrimitiveTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let inline private unixSeconds (timestamp:DateTimeOffset) = timestamp.ToUnixTimeSeconds() |> uint32

let create (timestamp:DateTimeOffset option) services (endpoint:IPEndPoint) =
  {
    timestamp = timestamp <!> unixSeconds
    services  = services
    ipAddress = endpoint.Address.MapToIPv6().GetAddressBytes()
    port      = uint16 endpoint.Port
  }

let read expectTimestamp buffer =
  if expectTimestamp then
    Buffer.readUint32LE buffer   >>= (fun (timestamp, buffer) ->
    Buffer.readUint64LE buffer   >>= (fun (services, buffer) ->
    Buffer.readBytes 16us buffer >>= (fun (ipBytes, buffer) ->
    Buffer.readUint16BE buffer   <!> (fun (port, remaining) ->
      let networkAddress =
        {
          timestamp = Some timestamp
          services  = services
          ipAddress = ipBytes
          port      = port
        }
      (networkAddress, remaining)
    ))))
  else
    Buffer.readUint64LE buffer   >>= (fun (services, buffer) ->
    Buffer.readBytes 16us buffer >>= (fun (ipBytes, buffer) ->
    Buffer.readUint16BE buffer   <!> (fun (port, remaining) ->
      let networkAddress =
        {
          timestamp = None
          services  = services
          ipAddress = ipBytes
          port      = port
        }
      (networkAddress, remaining)
    )))

let write (networkAddress:NetworkAddress) =
  fun buffer ->
       match networkAddress.timestamp with
       | Some timestamp -> Buffer.writeUint32LE timestamp buffer
       | None -> buffer
  >> Buffer.writeUint64LE networkAddress.services
  >> Buffer.writeBytes networkAddress.ipAddress
  >> Buffer.writeUint16BE networkAddress.port