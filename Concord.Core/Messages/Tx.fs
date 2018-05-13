module Tx

open MessageTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let create version inputs outputs lockTime =
  {
    version = version
    inputCount = Array.length inputs |> uint16
    inputs = inputs
    outputCount = Array.length outputs |> uint16
    outputs = outputs
    lockTime = lockTime
  }

let rec private readItems fn acc i buffer =
  if i = 0us then
    Some (acc, buffer)
  else
    match fn buffer with
    | None -> None
    | Some (item, buffer) -> readItems fn (item::acc) (i - 1us) buffer

let private readInputs = readItems TxInput.read []
let private readOutputs = readItems TxOutput.read []

let read buffer =
  Buffer.readInt32LE buffer      >>= (fun (version, buffer) ->
  VarInt.readUint16 buffer       >>= (fun (inputCount, buffer) ->
  readInputs inputCount buffer   >>= (fun (inputs, buffer) ->
  VarInt.readUint16 buffer       >>= (fun (outputCount, buffer) ->
  readOutputs outputCount buffer >>= (fun (outputs, buffer) ->
  Buffer.readUint32LE buffer     <!> (fun (lockTime, remaining) ->
    let tx =
      {
        version     = version
        inputCount  = inputCount
        inputs      = Array.ofList inputs
        outputCount = outputCount
        outputs     = Array.ofList outputs
        lockTime    = lockTime
      }
    (tx, remaining)
  ))))))
