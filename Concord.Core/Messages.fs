module Messages

open System
open System.IO
open System.Net
open System.Text

open MessageEnvelope
open Primitives

module Version =
    [<NoComparison>]
    type T =
        {
            ProtocolVersion: int32
            Services: uint64
            Timestamp: int64
            Receive: NetworkAddress.T
            Send: NetworkAddress.T
            Nonce: uint64
            UserAgent: string
            StartHeight: int32
            Relay: bool
        }

    let create relay startHeight userAgent nonce send receive timestamp services protocolVersion =
        {
            ProtocolVersion = protocolVersion
            Services = services
            Timestamp = timestamp
            Receive = receive
            Send = send
            Nonce = nonce
            UserAgent = userAgent
            StartHeight = startHeight
            Relay = relay
        }

    let wrap value =
        use buffer = new MemoryStream()
        use tx = new BinaryWriter(buffer)

        (Write.i32 value.ProtocolVersion
        >> Write.u64 value.Services
        >> Write.i64 value.Timestamp
        >> NetworkAddress.write value.Receive
        >> NetworkAddress.write value.Send
        >> Write.u64 value.Nonce
        >> Write.varstr Encoding.ASCII value.UserAgent
        >> Write.i32 value.StartHeight
        >> Write.bool value.Relay
        >> ignore) tx

        MessageEnvelope.create "version" (buffer.ToArray())

    let unwrap (envelope:MessageEnvelope.T) =
        use buffer = new MemoryStream(envelope.Payload)
        use rx = new BinaryReader(buffer)
        Read.From(rx) {
            try
                let! protocolVersion = Read.i32
                let! services = Read.u64
                let! timestamp = Read.i64
                let! receive = NetworkAddress.read false
                let! send = NetworkAddress.read false
                let! nonce = Read.u64
                let! userAgent = Read.varstr Encoding.ASCII
                let! startHeight = Read.i32
                let! relay = Read.bool

                return Ok (create relay startHeight userAgent nonce send receive timestamp services protocolVersion)
            with
            | :? IOException as ex -> return Error ex
        }

module Verack =
    let wrap () =
        MessageEnvelope.create "verack" (Array.empty)

    let unwrap (envelope:MessageEnvelope.T) =
        Ok ()

module Ping =
    type T = { Nonce: uint64 }
    
    let create nonce = { Nonce = nonce }

    let wrap value =
        let nonceBytes = BitConverter.GetBytes(value.Nonce) in
        MessageEnvelope.create "ping" nonceBytes

    let unwrap (envelope:MessageEnvelope.T) =
        let nonce = BitConverter.ToUInt64(envelope.Payload, 0) in
        Ok { Nonce = nonce }

module Pong =
    type T = { Nonce: uint64 }

    let create nonce = { Nonce = nonce }

    let wrap value =
        let nonceBytes = BitConverter.GetBytes(value.Nonce) in
        MessageEnvelope.create "pong" nonceBytes
    
    let unwrap (envelope:MessageEnvelope.T) =
        let nonce = BitConverter.ToUInt64(envelope.Payload, 0) in
        Ok { Nonce = nonce }

module GetHeaders =
    type T =
        {
            ProtocolVersion: uint32
            BlockLocatorHashes: byte array array
            HashStop: byte array
        }
    
    let create hashStop blockLocatorHashes protocolVersion =
        {
            ProtocolVersion = protocolVersion
            BlockLocatorHashes = blockLocatorHashes
            HashStop = hashStop
        }

    let unwrap (envelope:MessageEnvelope.T) =
        use buffer = new MemoryStream (envelope.Payload)
        use rx = new BinaryReader (buffer)

        Read.From(rx) {
            try
                let! version = Read.u32
                let! hashCount = Read.varint
                let blockLocatorHashes =
                    Array.zeroCreate (int hashCount)
                    |> Array.map (fun _ -> Read.u8s 32UL rx)
                let! hashStop = Read.u8s 32UL

                return Ok (create hashStop blockLocatorHashes version)
            with
             | :? IOException as ex -> return Error ex
        }

module Addr =
    [<NoComparison>]
    type T = { AddressList:NetworkAddress.T array }

    let create addresses = { AddressList = addresses }

    let unwrap (envelope:MessageEnvelope.T) =
        use buffer = new MemoryStream (envelope.Payload)
        use rx = new BinaryReader (buffer)

        Read.From(rx) {
            let! count = Read.varint
            let addresses =
                Array.zeroCreate (int count)
                |> Array.map (fun _ -> NetworkAddress.read true rx)
            
            return Ok (create addresses)
        }