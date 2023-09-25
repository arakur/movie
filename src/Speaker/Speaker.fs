namespace Speaker

open FSharp.Json

type SpeakerId = int

type Style =
    { [<JsonField("id")>]
      Id: SpeakerId }

type Speaker =
    { [<JsonField("speaker_uuid")>]
      SpeakerId: string
      [<JsonField("version")>]
      Version: string
      [<JsonField("styles")>]
      Styles: Map<string, Style> }

module Speaker =
    let private speakers =
        let json = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/speakers.json")
        let speakers = Json.deserialize<Map<string, Speaker>> json
        speakers

    let speaker name = speakers.[name]

    let speakerStyle name style = speakers.[name].Styles.[style]

    let id name style = speakers.[name].Styles.[style].Id
