open System

type Call = {
  StartedAt : DateTimeOffset
  CompletedAt : DateTimeOffset
}

[<Measure>]
type second

[<Measure>]
type paisa
let duration call =
  let ts =
    call.CompletedAt - call.StartedAt
  decimal (ts.TotalSeconds) * 1m<second>
  
// 
let charge (callDuration : decimal<second>) =
  callDuration * 2m<paisa/second>

let callCharge call =
  call
  |> duration
  |> charge


let aSampleCall = {
  StartedAt = DateTimeOffset.Now
  CompletedAt = DateTimeOffset.Now.AddSeconds(10.)
}

duration aSampleCall
callCharge aSampleCall