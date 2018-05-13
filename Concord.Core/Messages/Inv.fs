module Inv

open MessageTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let read buffer =
  let rec read' i acc buffer =
    if i = 0u then
      Some (acc, buffer)
    else
      match InventoryVector.read buffer with
      | None -> None
      | Some (hash, buffer) -> read' (i - 1u) (hash::acc) buffer

  VarInt.readUint32 buffer >>= (fun (count, buffer) ->
  read' count [] buffer <!> (fun (invs, remaining) ->
    let (inv:Inv) =
      {
        count = count
        inventory = invs
      }
    (inv, remaining)
  ))
