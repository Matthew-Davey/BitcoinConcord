module TxInput

open PrimitiveTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let create previousOutput signatureScript sequence =
  {
    previousOutput  = previousOutput
    scriptLength    = Array.length signatureScript |> uint32
    signatureScript = signatureScript
    sequence        = sequence
  }

let read buffer =
  OutPoint.read buffer                 >>= (fun (previousOutput, buffer) ->
  VarInt.readUint16 buffer             >>= (fun (scriptLength, buffer) ->
  Buffer.readBytes scriptLength buffer >>= (fun (signatureScript, buffer) ->
  Buffer.readUint32LE buffer           <!> (fun (sequence, remaining) ->
    let input =
      {
        previousOutput  = previousOutput
        scriptLength    = uint32 scriptLength
        signatureScript = signatureScript
        sequence        = sequence
      }
    (input, remaining)
  ))))
