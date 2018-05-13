module Envelope

open System
open System.Text

type Command = Command of string

type Envelope =
  {
    start   : uint32
    command : Command
    length  : uint32
    checksum: uint32
    payload : uint8 []
  }

let private encodeCommand (Command command) = command.PadRight(12, char 0) |> Encoding.ASCII.GetBytes
let private decodeCommand bytes = Command (Encoding.ASCII.GetString(bytes).TrimEnd(char 0))

let wrap start (Command command) messageBuffer =
  let checksum = Crypto.sha256d messageBuffer
                 |> Buffer.peek (uint16 sizeof<uint32>)
                 |> fun bytes -> BitConverter.ToUInt32(bytes, 0)
  {
    start    = start
    length   = uint32 (Buffer.length messageBuffer)
    command  = Command command
    checksum = checksum
    payload  = Buffer.toBytes messageBuffer
  }

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let read buffer =
  Buffer.readUint32LE buffer   >>= (fun (start, buffer) ->
  Buffer.readBytes 12us buffer >>= (fun (command, buffer) ->
  Buffer.readUint32LE buffer   >>= (fun (length, buffer) ->
  Buffer.readUint32LE buffer   >>= (fun (checksum, buffer) ->
  Buffer.readBytes (uint16 length) buffer <!> (fun (payload, remaining) ->
    let envelope =
      {
        start    = start
        command  = decodeCommand command
        length   = length
        checksum = checksum
        payload  = payload
      }
    (envelope, remaining)
  )))))

let readAll buffer =
  let rec readAll' acc buffer =
    match read buffer with
    | None -> (List.rev acc, buffer)
    | Some (envelope, remaining) -> readAll' (envelope::acc) remaining
  readAll' [] buffer

let write envelope =
  Buffer.writeUint32LE    envelope.start
  >> Buffer.writeBytes   (envelope.command |> encodeCommand)
  >> Buffer.writeUint32LE envelope.length
  >> Buffer.writeUint32LE envelope.checksum
  >> Buffer.writeBytes    envelope.payload

let toBuffer envelope = write envelope Buffer.empty
