#load "Result.fsx"

type Attribute = {
  Name : string
  Value : string
}

// Attribute -> string
let attrToHtmlStr attr =
  sprintf @"%s=""%s""" attr.Name attr.Value

let attr name value =
  {Name = name; Value = value}

let attrs nameValuePairs =
  nameValuePairs
  |> List.map (fun (n,v) -> attr n v)

let attrsToHtmlStr2 =
  List.map attrToHtmlStr 
  >> String.concat " "
// List<Attribute> -> string
let attrsToHtmlStr attrs =
  attrs
  |> List.map attrToHtmlStr 
  |> String.concat " "



type Element = {
  Name : string
  Attributes : Attribute list
  Content : Content option
} and Content =
  | PlainText of string
  | ChildElements of Element list

let plainText x = Some (PlainText x)
let childElements xs = Some (ChildElements xs)

let element name attributes c = 
  {Name = name; Attributes = attrs attributes; Content = c}

// Content -> string
let rec contentToHtmlStr content =
  match content with
  | PlainText x -> x
  | ChildElements xs ->
    xs 
    |> List.map elementToHtmlStr
    |> String.concat ""
and elementToHtmlStr element =
  let tag = element.Name
  let attrs = attrsToHtmlStr element.Attributes
  let contentOpt = 
    element.Content 
    |> Option.map contentToHtmlStr
  let content = defaultArg contentOpt ""
  sprintf "<%s %s> %s </%s>" 
    tag attrs content tag

let beveragesList = 
  let childElements =
    childElements [
      element "li" [] (plainText "Tea")
      element "li" [] (plainText "Coffee")]
  element "ul" [("class", "ulist")] childElements

// int -> Result<Element, string>
let getElementById id =
  if id = 1 then 
    Ok beveragesList
  else
    Error "unable to fetch"

// int -> Result<int, int>
let getHtmlStrById id =
  getElementById id
  |> Result.map elementToHtmlStr

let mergeAttrs attrs1 attrs2 =
  attrs1 @ attrs2
  |> List.groupBy (fun (a : Attribute) -> a.Name)
  |> List.map (fun (name, attrs) -> 
    let value = 
      attrs 
      |> List.map (fun a -> a.Value) 
      |> String.concat " "
    {Name = name; Value = value}
  )

let attrs1 = [{Name = "class"; Value = "col"}]
let attrs2 = 
  [{Name = "class"; Value = "s1"}
   {Name = "required"; Value = "required"}]

let mergeContent content1 content2 =
  match content1, content2 with
  | PlainText x, PlainText y ->
    sprintf "%s\n%s" x y
    |> PlainText
  | ChildElements xs, ChildElements ys ->
    ChildElements (xs @ ys)
  | _ -> content1

let mergeElement e1 e2 =
  let attrs = 
    mergeAttrs e1.Attributes e2.Attributes
  let content =
    Option.map2 mergeContent e1.Content e2.Content

  { Name = e1.Name
    Attributes = attrs
    Content = content}

let getAndMergeElements e1Id e2Id =
  let e1R = getElementById e1Id
  let e2R = getElementById e2Id
  Result.lift2 mergeElement e1R e2R  

type Configuration = {
  Id : int
  ElementId : int
}

// int -> Result<Configuration, string>
let getConfigurationById id =
  if id = 1 then
    Ok {Id = 1; ElementId = 1}
  else
    Error "unable to fetch"

let getElementByConfigId cId =
  getConfigurationById cId
  |> Result.bind (fun c ->
      getElementById c.ElementId
    )