module TxOutput

open PrimitiveTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let create value publicKeyScript =
  {
    value                 = value
    publicKeyScriptLength = Array.length publicKeyScript |> uint32
    publicKeyScript       = publicKeyScript
  }

let read buffer =
  Buffer.readInt64LE buffer            >>= (fun (value, buffer) ->
  VarInt.readUint16 buffer             >>= (fun (scriptLength, buffer) ->
  Buffer.readBytes scriptLength buffer <!> (fun (script, remaining) ->
    let output =
      {
        value = value
        publicKeyScriptLength = uint32 scriptLength
        publicKeyScript = script
      }
    (output, remaining)
  )))
