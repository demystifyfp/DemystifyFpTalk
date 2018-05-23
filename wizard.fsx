#load "prompt.fsx"
open Prompt 

let namePrompt : Prompt<string> = 
  stringPrompt "What's Your name?"

let agePrompt : Prompt<int> = 
  intPrompt "What's Your age?"

let twitterPrompt = stringPrompt "Twitter Handle?"
let facebookPrompt = stringPrompt "Facebook Name?"

let greeting name age =
  sprintf "Hey %s, you are %d years old" name age

let greetingWizard =
  lift2 greeting namePrompt agePrompt

let addAgeBy10Years age = age + 10

let prankWizard =
  agePrompt
  |> map addAgeBy10Years
  |> lift2 greeting namePrompt


type Demography = {
  Name : string
  Age : int
}

let demography name age = {
  Name = name
  Age = age
}

type Social = {
  Facebook : string
  Twitter : string
}

let social facebook twitter = {
  Facebook = facebook
  Twitter = twitter
}


type Profile = {
  Demography : Demography
  Social : Social
}

let profile demography social = {
  Demography = demography
  Social = social
}

let demographyWizard = 
  lift2 demography namePrompt agePrompt 

let socialWizard = 
  lift2 social facebookPrompt twitterPrompt 


let profileWizard =
  lift2 profile demographyWizard socialWizard
