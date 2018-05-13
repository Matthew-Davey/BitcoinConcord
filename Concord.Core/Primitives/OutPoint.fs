module OutPoint

open PrimitiveTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let create hash index =
  {
    hash = hash
    index = index
  }

let read buffer =
  Buffer.readBytes 32us buffer >>= (fun (hash, buffer) ->
  Buffer.readUint32LE buffer <!> (fun (index, remaining) ->
    let outpoint =
      {
        hash = hash
        index = index
      }
    (outpoint, remaining)
  ))

let write message =
  Buffer.writeBytes message.hash
  >> Buffer.writeUint32LE message.index
