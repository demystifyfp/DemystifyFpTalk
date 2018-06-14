#load "Result.fsx"
open System


type Call = {
  StartedAt : DateTimeOffset
  CompletedAt: DateTimeOffset
}

[<Measure>]
type second

[<Measure>]
type paisa

type Customer = {
  Plan : decimal<paisa/second>
}

type SimStatus =
| Active
| InActive of deactivatedAt:DateTimeOffset

let duration (call : Call) =
  let ts = call.CompletedAt - call.StartedAt
  decimal(ts.TotalDays) * 1m<second>

let charge (plan : decimal<paisa/second>) (callDuration : decimal<second>) =
  callDuration * plan

let callCharge (customer : Customer) call =
  let chargedBasedPlan = charge customer.Plan
  call
  |> duration
  |> chargedBasedPlan


let callCharges (customer : Customer) calls =
  calls
  |> List.map (callCharge customer)

let customer = { Plan  = 0.75m<paisa/second>}

let getCustomer id =
  if id = 1 then
    Ok customer 
  else
    Error "Db down!"


let formatterPlan (customer : Customer) =
  sprintf "%A paisa per second" customer.Plan


let getCustomerPlan id =
  getCustomer id
  |> Result.map formatterPlan

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

let getCustomerCalls id =
  if id = 1 then
    Ok aListOfCalls
  else
    Error "db down!"

let getCustomerCallsCharges id =
  let customer = getCustomer id
  let calls = getCustomerCalls id
  Result.lift2 callCharges customer calls

let getCustomerSimStatus (customer : Customer) =
  // 



