module MessageTypes

open PrimitiveTypes

type GetData =
  {
    count:uint32
    inventory:InventoryVector list
  }

type Inv =
  {
    count:uint32
    inventory:InventoryVector list
  }

type PingPong =
  {
    nonce:uint64
  }

type Tx =
  {
    version:int32
    inputCount:uint16
    inputs:TxInput[]
    outputCount:uint16
    outputs:TxOutput[]
    lockTime:uint32
  }

type Version =
  {
    protocolVersion:int32
    services:uint64
    timestamp:int64
    receivingAddress:NetworkAddress
    sendingAddress:NetworkAddress
    nonce:uint64
    userAgent:string
    startHeight:int32
    relay:bool
  }
