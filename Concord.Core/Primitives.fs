module Primitives

open System
open System.IO
open System.Net
open System.Text

module Read =
    type From(rx:BinaryReader) =
        member _this.Bind(x, f) = f (x rx)
        member _this.Return(x) = x
        member _this.ReturnFrom(x) = x rx
        member this.TryWith(body, handler) =
            try
               this.Return(body) 
            with e -> handler e
        member this.Delay(f) = f()

    let u8 (rx:BinaryReader) = rx.ReadByte()
    let u8s (count:uint64) (rx:BinaryReader) = rx.ReadBytes(int count)
    let u64 (rx:BinaryReader) = rx.ReadUInt64()
    let u32 (rx:BinaryReader) = rx.ReadUInt32()
    let u16 (rx:BinaryReader) = rx.ReadUInt16()
    let i64 (rx:BinaryReader) = rx.ReadInt64()
    let i32 (rx:BinaryReader) = rx.ReadInt32()
    let i16 (rx:BinaryReader) = rx.ReadInt16()
    let bool (rx:BinaryReader) = rx.ReadBoolean()

    let varint rx =
        From(rx) {
            let! discriminator = u8
            match discriminator with
            | 255uy -> return! u64
            | 254uy -> return! u32 >> uint64
            | 253uy -> return! u16 >> uint64
            | _ -> return uint64 discriminator
        }

    let varstr (encoding:Encoding) rx =
        From(rx) {
            let! length = varint
            let! bytes  = u8s length

            return encoding.GetString bytes
        }

module Write =
    let u8 (x:byte) (tx:BinaryWriter) = tx.Write(x); tx
    let u8s (x:byte array) (tx:BinaryWriter) = tx.Write(x); tx
    let u64 (x:uint64) (tx:BinaryWriter) = tx.Write(x); tx
    let u32 (x:uint32) (tx:BinaryWriter) = tx.Write(x); tx
    let u16 (x:uint16) (tx:BinaryWriter) = tx.Write(x); tx
    let i32 (x:int32) (tx:BinaryWriter) = tx.Write(x); tx
    let i64 (x:int64) (tx:BinaryWriter) = tx.Write(x); tx
    let bool (x:bool) (tx:BinaryWriter) = tx.Write(x); tx

    let varint v =
        match v with
        | v when v < 253UL ->
            u8 (byte v)
        | v when v < (uint64 System.UInt16.MaxValue) ->
            u8 253uy >> u16 (uint16 v)
        | v when v < (uint64 System.UInt32.MaxValue) ->
            u8 254uy >> u32 (uint32 v)
        | _ -> u8 255uy >> u64 v

    let varstr (encoding:Encoding) (v:String) =
        let length = uint64 (encoding.GetByteCount (v))
        let bytes = encoding.GetBytes (v)
        varint length >> u8s bytes
     
module NetworkAddress =
    [<NoComparison>]
    type T =
        {
            Timestamp: uint32 option
            Services: uint64
            Endpoint: IPEndPoint
        }

    let create endpoint services timestamp =
        {
            Timestamp = timestamp
            Services = services
            Endpoint = endpoint
        }

    let empty = create (IPEndPoint(IPAddress.Any, 0)) 1UL None

    let private writeIPEndpoint (v:IPEndPoint) =
        let address = v.Address.MapToIPv6()
        let addressBytes = address.GetAddressBytes()
        let portBytes = BitConverter.GetBytes(uint16 v.Port) |> Array.rev
        Write.u8s addressBytes
        >> Write.u8s portBytes

    let private readIPEndpoint rx =
        Read.From(rx) {
            let! addressBytes = Read.u8s 16UL
            let! portBytes =  Read.u8s (uint64 sizeof<uint16>) >> Array.rev
            return IPEndPoint (IPAddress(addressBytes), int (BitConverter.ToUInt16 (portBytes, 0)))
        }

    let write value =
        match value.Timestamp with
        | Some timestamp ->
            Write.u32 timestamp
            >> Write.u64 value.Services
            >> writeIPEndpoint value.Endpoint
        | None ->
            Write.u64 value.Services
            >> writeIPEndpoint value.Endpoint

    let read includeTimestamp rx =
        Read.From(rx) {
            match includeTimestamp with
            | true ->
                let! timestamp = Read.u32 >> Some
                let! services = Read.u64
                let! endpoint = readIPEndpoint

                return create endpoint services timestamp
            | false ->
                let! services = Read.u64
                let! endpoint = readIPEndpoint

                return create endpoint services None
        }
