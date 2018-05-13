module Buffer

open System
open System.Globalization
open System.IO

type Buffer = Buffer of uint8 array

let empty = Buffer Array.empty

let inline toBytes (Buffer buffer) = buffer

let ofHexString hex =
  if String.length hex % 2 = 0
    then
      hex.ToCharArray()
      |> Array.chunkBySize 2
      |> Array.map (fun nibbles -> Byte.Parse(string nibbles, NumberStyles.HexNumber))
      |> Buffer
      |> Ok
    else
      Error (FormatException("hex length must be a multiple of 2"))

let toHexString (Buffer buffer) =
  buffer
  |> Array.map (fun byte -> byte.ToString("x2"))
  |> String.concat ""

let length (Buffer buffer) = uint16 (Array.length buffer)
let peek (count:uint16) (Buffer buffer) = Array.take (int count) buffer

let appendBytes bytes (Buffer buffer) = Array.append buffer bytes |> Buffer
let appendBuffer (Buffer b) (Buffer a) = Array.append a b |> Buffer

let read (count:uint16) (stream:Stream) =
  try
    let bytes = Array.zeroCreate (int count)
    stream.Read(bytes, 0, int count) |> ignore
    Ok (Buffer bytes)
  with
  | exn -> Error exn

let write (stream:Stream) (Buffer buffer) =
  try
    Ok (stream.Write(buffer, 0, Array.length buffer))
  with
  | exn -> Error exn

let inline private bytesToBoolean bytes = BitConverter.ToBoolean(bytes, 0)
let inline private bytesToInt16 bytes  = BitConverter.ToInt16(bytes, 0)
let inline private bytesToInt32 bytes  = BitConverter.ToInt32(bytes, 0)
let inline private bytesToInt64 bytes  = BitConverter.ToInt64(bytes, 0)
let inline private bytesToUint16 bytes = BitConverter.ToUInt16(bytes, 0)
let inline private bytesToUint32 bytes = BitConverter.ToUInt32(bytes, 0)
let inline private bytesToUint64 bytes = BitConverter.ToUInt64(bytes, 0)

let inline private boolToBytes (value:bool)     = BitConverter.GetBytes(value)
let inline private int16ToBytes (value:int16)   = BitConverter.GetBytes(value)
let inline private int32ToBytes (value:int32)   = BitConverter.GetBytes(value)
let inline private int64ToBytes (value:int64)   = BitConverter.GetBytes(value)
let inline private uint16ToBytes (value:uint16) = BitConverter.GetBytes(value)
let inline private uint32ToBytes (value:uint32) = BitConverter.GetBytes(value)
let inline private uint64ToBytes (value:uint64) = BitConverter.GetBytes(value)

let private read' count transform (Buffer buffer) =
  if count > (Array.length buffer) then None
  else
    buffer
    |> Array.splitAt count
    |> fun (bytes, remaining) -> Some (transform bytes, Buffer remaining)

let readBytes (count:uint16) = read' (int count) id
let readBool     = read' sizeof<bool> bytesToBoolean
let readUint8    = read' sizeof<uint8> Array.exactlyOne
let readInt16LE  = read' sizeof<int16> bytesToInt16
let readInt32LE  = read' sizeof<int32> bytesToInt32
let readInt64LE  = read' sizeof<int64> bytesToInt64
let readInt16BE  = read' sizeof<int16> (Array.rev >> bytesToInt16)
let readInt32BE  = read' sizeof<int32> (Array.rev >> bytesToInt32)
let readInt64BE  = read' sizeof<int64> (Array.rev >> bytesToInt64)
let readUint16LE = read' sizeof<uint16> bytesToUint16
let readUint32LE = read' sizeof<uint32> bytesToUint32
let readUint64LE = read' sizeof<uint64> bytesToUint64
let readUint16BE = read' sizeof<uint16> (Array.rev >> bytesToUint16)
let readUint32BE = read' sizeof<uint32> (Array.rev >> bytesToUint32)
let readUint64BE = read' sizeof<uint64> (Array.rev >> bytesToUint64)

let writeBytes    = appendBytes
let writeBool     = boolToBytes >> appendBytes
let writeUint8    = Array.singleton >> appendBytes
let writeInt16LE  = int16ToBytes >> appendBytes
let writeInt32LE  = int32ToBytes >> appendBytes
let writeInt64LE  = int64ToBytes >> appendBytes
let writeInt16BE  = int16ToBytes >> Array.rev >> appendBytes
let writeInt32BE  = int32ToBytes >> Array.rev >> appendBytes
let writeInt64BE  = int64ToBytes >> Array.rev >> appendBytes
let writeUint16LE = uint16ToBytes >> appendBytes
let writeUint32LE = uint32ToBytes >> appendBytes
let writeUint64LE = uint64ToBytes >> appendBytes
let writeUint16BE = uint16ToBytes >> Array.rev >> appendBytes
let writeUint32BE = uint32ToBytes >> Array.rev >> appendBytes
let writeUint64BE = uint64ToBytes >> Array.rev >> appendBytes