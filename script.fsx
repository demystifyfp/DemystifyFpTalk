open System

type Call = {
  StartedAt : DateTimeOffset
  CompletedAt : DateTimeOffset
}

[<Measure>]
type second

[<Measure>]
type paisa

type Customer = {
  Plan : decimal<paisa/second>
}

let duration call =
  let ts =
    call.CompletedAt - call.StartedAt
  decimal (ts.TotalSeconds) * 1m<second>
  

let charge (plan : decimal<paisa/second>) (callDuration : decimal<second>) =
  callDuration * plan

let callCharge customer call =
  call
  |> duration
  |> charge (customer.Plan)


let aSampleCall = {
  StartedAt = DateTimeOffset.Now
  CompletedAt = DateTimeOffset.Now.AddSeconds(10.)
}

let customer = {
  Plan = 0.5m<paisa/second>
}

duration aSampleCall
callCharge customer aSampleCall