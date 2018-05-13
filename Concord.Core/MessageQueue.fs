module MessageQueue

open System.Text
open RabbitMQ.Client
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open Newtonsoft.Json.Serialization

let private serializerSettings =
  new JsonSerializerSettings(
    Formatting       = Formatting.None,
    Converters       = List.toArray [new StringEnumConverter()],
    ContractResolver = new CamelCasePropertyNamesContractResolver()
  )
let makePublisherAgent publicationAddress (model:IModel) = MailboxProcessor.Start(fun inbox ->
  let rec messageLoop() = async {
    let! (payload, properties:IBasicProperties) = inbox.Receive()

    let serializedPayload = JsonConvert.SerializeObject(payload, serializerSettings)
    let payloadBytes = Encoding.UTF8.GetBytes(serializedPayload)

    model.BasicPublish(publicationAddress, properties, payloadBytes)

    return! messageLoop()
  }
  messageLoop()
)
