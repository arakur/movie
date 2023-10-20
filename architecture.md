# Architecture

::: mermaid
flowchart LR

classDef Resource fill:#ddd,stroke:#aac;

classDef InnerResource fill:#fff,stroke:#aac;

classDef Flow fill:#fcf,stroke:#a2c,stroke-width:2px;

classDef ExternalProgram fill:#fca,stroke:#caa,stroke-width:2px;

classDef FlowGroup fill:#fff,stroke:#aac,stroke-width:2px,stroke-dasharray: 5, 5;

classDef None fill:#fff0,stroke:#fff0;

Script(Script)
class Script Resource

AssetFile1(Asset File 1)
class AssetFile1 Resource

AssetFile2(Asset File 2)
class AssetFile2 Resource

subgraph Syntax
  Tokenize[Tokenize]
  class Tokenize Flow
  
  Parse[Parse]
  class Parse Flow
end
class Syntax FlowGroup

AST(AST)
class AST InnerResource

Script .-> Tokenize --> Parse .-> AST

Interpret[Interpret]
class Interpret Flow

subgraph FramesData[ Frames Data ]
  subgraph Frames
    direction TB

    Frame1(Frame 1)~~~Frame2(Frame 2)~~~FrameRest(⋮)
    class Frame1,Frame2 InnerResource
    class FrameRest None
  end
  subgraph Assets
    direction TB

    Asset1(Asset 1)~~~Asset2(Asset 2)~~~AssetRest(⋮)
    class Asset1,Asset2 InnerResource
    class AssetRest None
  end
end
class FramesData InnerResource

AST .-> Interpret .-> FramesData

subgraph ExportVideo[Export Video]
  GenerateIntermediateMedia[Generate Intermediate Media]
  class GenerateIntermediateMedia Flow

  subgraph Intermediate
    subgraph Subtitle
      GenerateSubtitles[Generate Subtitles]
      --> Typst{{Typst}} --> ImageMagickSubtitle{{ImageMagick}}
      .-> Subtitle1(Subtitle 1) & Subtitle2(Subtitle 2) & SubtitleRest(⋮)
      class GenerateSubtitles Flow
      class Typst ExternalProgram
      class ImageMagickSubtitle ExternalProgram
      class Subtitle1 InnerResource
      class Subtitle2 InnerResource
      class SubtitleRest None
    end
    class Subtitle FlowGroup

    subgraph Voice
      GenerateVoice[Generate Voice]
      --> Voicevox{{Voicevox}}
      ..-> Voice1(Voice 1) & Voice2(Voice 2) & VoiceRest(⋮)
      class GenerateVoice Flow
      class Voicevox ExternalProgram
      class Voice1 InnerResource
      class Voice2 InnerResource
      class VoiceRest None
    end
    class Voice FlowGroup

    subgraph Appearance
      GenerateAppearance[Generate Appearance]
      --> ImageMagickAppearance{{ImageMagick}}
      ..-> Appearance1(Appearance 1) & Appearance2(Appearance 2) & AppearanceRest(⋮)
      class GenerateAppearance Flow
      class ImageMagickAppearance ExternalProgram
      class Appearance1 InnerResource
      class Appearance2 InnerResource
      class AppearanceRest None
    end
    class Appearance FlowGroup
  end
  class Intermediate FlowGroup

  Compose[Compose]
  class Compose Flow

  FFmpegCompose{{FFmpeg}}
  class FFmpegCompose ExternalProgram
end
class ExportVideo FlowGroup

FramesData .-> GenerateIntermediateMedia --> GenerateSubtitles & GenerateVoice & GenerateAppearance -----> Compose --> FFmpegCompose

Subtitle1 & Subtitle2 & Voice1 & Voice2 & Appearance1 & Appearance2 .-> FFmpegCompose
AssetFile1 & AssetFile2 ............-> FFmpegCompose

Output(Output)
class Output Resource

FFmpegCompose .-> Output
:::
