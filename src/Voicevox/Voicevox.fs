namespace Voicevox

open VoicevoxCore
open Speaker

//

type Speech =
    { Speaker: string
      Style: string
      Talk: string }

    member this.SpeakerId = Speaker.id this.Speaker this.Style

type SynthesisResult = Result<Data.Wav, string>

//

type Voicevox() =
    let core = new VoicevoxCore()

    member __.Synthesize(speech: Speech) =
        let id = speech.SpeakerId
        let struct (bytes, isSucceed) = core.Synthesize(speech.Talk, id)

        if isSucceed then
            Ok(Data.Wav.Create bytes)
        else
            Error "Failed to synthesize"

    interface System.IDisposable with
        member __.Dispose() = core.Dispose()
