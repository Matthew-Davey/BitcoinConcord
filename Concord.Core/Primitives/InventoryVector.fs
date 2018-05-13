module InventoryVector

open PrimitiveTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let read buffer =
  Buffer.readUint32LE buffer   >>= (fun (objectType, buffer) ->
  Buffer.readBytes 32us buffer <!> (fun (hash, remaining) ->
    let inventoryVector =
      {
        objectType = objectType
        hash = hash
      }
    (inventoryVector, remaining)
  ))

let create objectType hash =
  {
    objectType = objectType
    hash = hash
  }

let write (value:InventoryVector) =
  Buffer.writeUint32LE value.objectType
  >> Buffer.writeBytes value.hash
