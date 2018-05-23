open System

type PromptError = 
| ParseError

type Prompt<'a> = unit -> Result<'a,PromptError>

let map f (x : Prompt<'a>)  = 
  fun () -> 
    match x () with
    | Ok y -> f y |> Ok
    | Error e -> Error e

let bind (f : 'a -> Prompt<'b>) (p : Prompt<'a>) : Prompt<'b> = 
  match p () with
  | Ok x -> f x
  | Error e -> fun () -> Error e

let apply p1 p2 = 
  fun () ->
    match p1 () with
    | Ok f -> 
      match p2 () with
      | Ok y -> y |> f |> Ok
      | Error e -> Error e
    | Error e -> Error e

let lift2 (f : 'a -> 'b -> 'c) (x : Prompt<'a>) (y : Prompt<'b>) =
  apply (map f x) y

let parseWith f value =
  match f value with
  | true, v -> Ok v
  | _ -> Error ParseError

let parseInt = parseWith Int32.TryParse
let parseString = parseWith (fun (v : string) -> true, v)

let promptAndParse parse message =
  fun () -> 
    printfn "%s" message
    Console.ReadLine() 
    |> parse 

let intPrompt = promptAndParse parseInt
let stringPrompt = promptAndParse parseString

let noPrompt = Ok ()