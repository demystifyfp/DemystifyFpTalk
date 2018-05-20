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

let aListOfCalls = 
  [{
    StartedAt = DateTimeOffset.Parse "20/05/2018 11:05AM"
    CompletedAt = DateTimeOffset.Parse "20/05/2018 11:10AM"}
   {
    StartedAt = DateTimeOffset.Parse "20/05/2018 12:02PM"
    CompletedAt = DateTimeOffset.Parse "20/05/2018 12:03PM"} 
   {
    StartedAt = DateTimeOffset.Parse "20/05/2018 01:01:10PM"
    CompletedAt = DateTimeOffset.Parse "20/05/2018 01:01:25PM"}]

duration aSampleCall
callCharge customer aSampleCall