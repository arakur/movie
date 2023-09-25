#i "nuget: https://api.nuget.org/v3/index.json"
#r "nuget: FSharp.Data"

open FSharp.Data

type Json = JsonProvider<"./metas.json">

type T = Json.Root

let metas = Json.GetSamples()

let metaToJson (meta: T) =
    let uuid = meta.SpeakerUuid.ToString()
    let version = meta.Version

    let styles =
        meta.Styles
        |> Array.map (fun style -> style.Name, [| "id", style.Id |> decimal |> JsonValue.Number |] |> JsonValue.Record)

    [| "speaker_uuid", JsonValue.String uuid
       "version", JsonValue.String version
       "styles", JsonValue.Record styles |]

let map =
    metas
    |> Array.map (fun meta -> meta.Name, meta |> metaToJson |> JsonValue.Record)
    |> JsonValue.Record

System.IO.File.WriteAllText(__SOURCE_DIRECTORY__ + "/speakers.json", map.ToString())
