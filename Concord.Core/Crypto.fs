module Crypto

open System.Security.Cryptography
open Buffer

let sha256 (Buffer buffer) =
  use hash = SHA256.Create()
  Buffer (hash.ComputeHash buffer)

let sha256d = sha256 >> sha256
