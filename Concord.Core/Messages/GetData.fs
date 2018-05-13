module GetData

open MessageTypes

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let create (count) (inventory):GetData=
  {
    count = count
    inventory = inventory
  }

let write (message:GetData) =
  let rec write' inventory buffer =
    match inventory with
    | x::xs ->
        let buffer = InventoryVector.write x buffer
        write' xs buffer
    | [] -> buffer

  VarInt.writeUint32 message.count
  >> write' message.inventory

let toBuffer message = write message (Buffer.empty)
