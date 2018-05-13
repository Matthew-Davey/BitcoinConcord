module PrimitiveTypes

type InventoryVector =
  {
    objectType:uint32
    hash:uint8[]
  }

type NetworkAddress =
  {
    timestamp:uint32 option
    services:uint64
    ipAddress:uint8[]
    port:uint16
  }

type OutPoint =
  {
    hash:uint8[]
    index:uint32
  }

type TxInput =
  {
    previousOutput:OutPoint
    scriptLength:uint32
    signatureScript:uint8[]
    sequence:uint32
  }

type TxOutput =
  {
    value:int64
    publicKeyScriptLength:uint32
    publicKeyScript:uint8[]
  }
