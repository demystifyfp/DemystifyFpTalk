#load "Result.fsx"

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
  SimNumber : string
}

type SimStatus =
  | Active
  | InActive of DateTimeOffset

let duration call =
  let ts =
    call.CompletedAt - call.StartedAt
  decimal (ts.TotalSeconds) * 1m<second>
  

let charge (plan : decimal<paisa/second>) (callDuration : decimal<second>) =
  callDuration * plan

let callCharge customer call =
  call
  |> duration
  |> charge customer.Plan

let callCharges customer calls =
  calls
  |> List.map (callCharge customer)



let aSampleCall = {
  StartedAt = DateTimeOffset.Now
  CompletedAt = DateTimeOffset.Now.AddSeconds(10.)
}

let getCustomer id = 
  if id = 1 then
    Ok {SimNumber = "S345"; Plan = 0.75m<paisa/second>}
  else
    Error "No DB Connection available"

let getCustomerPlan id =
  
  let customerPlan (c : Customer) =
    sprintf "%.2f paisa per second" c.Plan
  
  getCustomer id
  |> Result.map customerPlan


let getCustomerSimStatus customer =
  match customer.SimNumber with
  | "S123" -> Ok Active
  | "S345" -> Ok (InActive (DateTimeOffset.Parse "20/05/2018 11:05AM"))
  | _ -> Error "Db Down!"

let getCustomerSimStatusById id =
  getCustomer id
  |> Result.bind getCustomerSimStatus
  |> Result.map (function
    | Active -> sprintf "Active"
    | InActive dateTime -> sprintf "InActive (%s)" (dateTime.ToString("MMM dd, yyyy"))
  )

let customer = {
  SimNumber = "S123"
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


let getCustomerCalls id = 
  if id = 1 then
    Ok aListOfCalls
  else  
    Error "db down!"

duration aSampleCall
callCharge customer aSampleCall
callCharges customer aListOfCalls

