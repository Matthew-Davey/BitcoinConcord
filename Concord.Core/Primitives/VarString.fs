module VarString

open System.Text

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let read buffer =
  VarInt.readUint16 buffer >>= (fun (length, buffer) ->
  Buffer.readBytes length buffer <!> (fun (bytes, remaining) ->
    (Encoding.ASCII.GetString(bytes), remaining)
  ))

let write (string:string) =
  VarInt.writeUint16 (uint16 (Encoding.UTF8.GetByteCount(string)))
  >> Buffer.writeBytes (Encoding.UTF8.GetBytes(string))