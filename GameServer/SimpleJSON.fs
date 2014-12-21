module SimpleJSON

open GameWorld

let toJson = Newtonsoft.Json.JsonConvert.SerializeObject
                        