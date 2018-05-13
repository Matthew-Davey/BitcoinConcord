module MessageEnvelope

open System
open System.IO
open System.Text

open Crypto
open Primitives

type T = { Command:string; Payload:byte array }

let create command payload =
    {
        Command = command
        Payload = payload
    }

type Network =
    | Mainnet
    | Testnet

let private networkMagic = function
    | Mainnet -> 3652501241u
    | Testnet -> 3669344250u

let private encode (command:string) = command.PadRight(12, char 0) |> Encoding.ASCII.GetBytes
let private decode bytes = Encoding.ASCII.GetString(bytes).TrimEnd(char 0)

let private calcChecksum payload =
    let hashedPayload = sha256d payload in
    BitConverter.ToUInt32 (hashedPayload, 0)

let write stream network value =
    use buffer = new MemoryStream()
    use tx = new BinaryWriter(buffer)

    (Write.u32 (networkMagic network)
    >> Write.u8s (encode value.Command)
    >> Write.u32 (uint32 (Array.length value.Payload))
    >> Write.u32 (calcChecksum value.Payload)
    >> Write.u8s value.Payload
    >> ignore) tx

    try
        buffer.Seek(0L, SeekOrigin.Begin) |> ignore
        Console.WriteLine(buffer.ToArray() |> Array.map (fun byte -> byte.ToString("x2")) |> (String.concat ""))
        buffer.CopyTo stream
        Ok ()
    with
    | :? IOException as ex -> Error ex

type DeserializeError =
    | InvalidMagic
    | InvalidChecksum

let read stream network =
    let validateMagic rx =
        let magic = Read.u32 rx in
        match magic with
        | i when i = (networkMagic network) -> Ok (rx)
        | _ -> Error InvalidMagic

    let readPayload rx =
        Read.From(rx) {
            let! command  = Read.u8s 12UL
            let! length   = Read.u32
            let! checksum = Read.u32
            let! payload  = Read.u8s (uint64 length)

            if checksum <> calcChecksum payload then return Error InvalidChecksum
            else return Ok {
                Command = decode command
                Payload = payload
            }
        }

    validateMagic (new BinaryReader (stream, Encoding.Default, true))
    |> Result.bind readPayload
