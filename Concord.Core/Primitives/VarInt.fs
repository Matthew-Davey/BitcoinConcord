module VarInt

open System

let inline private (>>=) m fn = Option.bind fn m
let inline private (<!>) m fn = Option.map fn m

let readUint8 buffer =
  Buffer.readUint8 buffer >>= fun (discriminator, buffer) ->
    match discriminator with
    | 255uy -> None
    | 254uy -> None
    | 253uy -> Buffer.readUint16LE buffer >>= fun (value, buffer) ->
        match value with
          | value when value < 256us -> Some (uint8 value, buffer)
          | _ -> None
    | _ -> Some (discriminator, buffer)

let readUint16 buffer =
  Buffer.readUint8 buffer >>= fun (discriminator, buffer) ->
    match discriminator with
    | 255uy -> None
    | 254uy -> None
    | 253uy -> Buffer.readUint16LE buffer
    | _ -> Some (uint16 discriminator, buffer)

let readUint32 buffer =
  Buffer.readUint8 buffer >>= fun (discriminator, buffer) ->
    match discriminator with
    | 255uy -> None
    | 254uy -> Buffer.readUint32LE buffer
    | 253uy -> Buffer.readUint16LE buffer <!> fun (value, buffer) -> (uint32 value, buffer)
    | _ -> Some (uint32 discriminator, buffer)

let readUint64 buffer =
  Buffer.readUint8 buffer >>= fun (discriminator, buffer) ->
    match discriminator with
    | 255uy -> Buffer.readUint64LE buffer
    | 254uy -> Buffer.readUint32LE buffer <!> fun (value, buffer) -> (uint64 value, buffer)
    | 253uy -> Buffer.readUint16LE buffer <!> fun (value, buffer) -> (uint64 value, buffer)
    | _ -> Some (uint64 discriminator, buffer)

let writeUint8 (value:uint8) buffer =
  match value with
  | value when value <= 253uy -> Buffer.writeUint8 value buffer
  | _ -> Buffer.writeUint16LE (uint16 value) buffer

let writeUint16 (value:uint16) buffer =
  match value with
  | value when value <= 253us -> Buffer.writeUint8 (uint8 value) buffer
  | _ -> Buffer.writeUint16LE value buffer

let writeUint32 (value:uint32) buffer =
  match value with
  | value when value <= 253u -> Buffer.writeUint8 (uint8 value) buffer
  | value when value <= uint32 UInt16.MaxValue -> Buffer.writeUint16LE (uint16 value) buffer
  | _ -> Buffer.writeUint32LE value buffer

let writeUint64 (value:uint64) buffer =
  match value with
  | value when value <= 253UL -> Buffer.writeUint8 (uint8 value) buffer
  | value when value <= uint64 UInt16.MaxValue -> Buffer.writeUint16LE (uint16 value) buffer
  | value when value <= uint64 UInt32.MaxValue -> Buffer.writeUint32LE (uint32 value) buffer
  | _ -> Buffer.writeUint64LE value buffer
