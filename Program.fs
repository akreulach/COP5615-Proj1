open Suave
open Suave.Operators
open Suave.Filters
open Suave.Logging

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

let ws (webSocket : WebSocket) (context: HttpContext) =
  socket {
    let mutable loop = truewhile loop do
      let! msg = webSocket.read()match msg with
      | (Text, data, true) ->
        let str = UTF8.toString data
        let response = sprintf "response to %s" strlet byteResponse =
          response
          |> System.Text.Encoding.UTF8.GetBytes
          |> ByteSegmentdo! webSocket.send Text byteResponse true| (Close, _, _) ->
        let emptyResponse = [||] |> ByteSegment
        do! webSocket.send Close emptyResponse true
        loop <- false
      | _ -> ()
    }let app: WebPart =
    choose [
        path "/websocket" >=> handShake ws
    ][<EntryPoint>]
let main _ = 
    startWebServer {defaultConfig with logger = Targets.create Verbose [||] } app
    0