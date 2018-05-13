module UnknownMessageAgent

let create () =
  MailboxProcessor.Start (fun inbox ->
    let rec messageLoop () = async {
      let! (_, message) = inbox.Receive()
      printfn "%A" message
      return! messageLoop()
    }
    messageLoop()
  )
