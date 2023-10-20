# Architecture

::: mermaid
flowchart

%% styles %%

classDef Resource fill:#ddd,stroke:#aac;

classDef InnerResource fill:#fff,stroke:#aac;

classDef ResourceGroup fill:#eff,stroke:#aac,stroke-width:2px;

classDef Flow fill:#fcf,stroke:#a2c,stroke-width:2px;

classDef ExternalProgram fill:#fca,stroke:#caa,stroke-width:2px;

classDef FlowGroup fill:#fffcf8,stroke:#aac,stroke-width:2px,stroke-dasharray: 5, 5;

classDef None fill:#fff0,stroke:#fff0;

%% nodes %%

subgraph UserResources[User Resources]
  Script(Script)
  class Script Resource

  AssetFile1(Asset File 1)
  class AssetFile1 Resource

  AssetFile2(Asset File 2)
  class AssetFile2 Resource

  AssetFileRest(...)
  class AssetFileRest None

  AppearanceResource1(Appearance Resource 1)
  class AppearanceResource1 Resource

  AppearanceResource2(Appearance Resource 2)
  class AppearanceResource2 Resource

  AppearanceResourceRest(...)
  class AppearanceResourceRest None
end
class UserResources ResourceGroup

subgraph Syntax
  Tokenize[Tokenize]
  class Tokenize Flow
  
  Parse[Parse]
  class Parse Flow
end
class Syntax FlowGroup

AST(AST)
class AST InnerResource

Script ..- Tokenize === Parse .-> AST

subgraph InterpretGroup[Interpret]
  Interpret[Interpret]
  class Interpret Flow
end
class InterpretGroup FlowGroup

subgraph FramesData[ Frames Data ]
  subgraph Frames
    direction TB

    Frame1(Frame 1)---Frame2(Frame 2)---FrameRest(...)
    class Frame1,Frame2 InnerResource
    class FrameRest None
  end
  subgraph Assets
    direction TB

    Asset1(Asset 1)---Asset2(Asset 2)---AssetRest(...)
    class Asset1,Asset2 InnerResource
    class AssetRest None
  end
end
class FramesData InnerResource

AST ..- Interpret .-> FramesData

subgraph ExportVideo[Export Video]
  GenerateIntermediateMedia[Generate Intermediate Media]
  class GenerateIntermediateMedia Flow

  subgraph Intermediate
    subgraph Subtitle
      GenerateSubtitles[Generate Subtitles]
      --x Typst{{Typst}} --x ImageMagickSubtitle{{ImageMagick}}
      .-> Subtitle1(Subtitle 1) & Subtitle2(Subtitle 2) & SubtitleRest(...)
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
      --x Voicevox{{Voicevox}}
      ..-> Voice1(Voice 1) & Voice2(Voice 2) & VoiceRest(...)
      class GenerateVoice Flow
      class Voicevox ExternalProgram
      class Voice1 InnerResource
      class Voice2 InnerResource
      class VoiceRest None
    end
    class Voice FlowGroup

    subgraph Appearance
      GenerateAppearance[Generate Appearance]
      --x ImageMagickAppearance{{ImageMagick}}
      ..-> Appearance1(Appearance 1) & Appearance2(Appearance 2) & AppearanceRest(...)
      AppearanceResource1 ..- ImageMagickAppearance
      AppearanceResource2 ..- ImageMagickAppearance
      AppearanceResourceRest ..- ImageMagickAppearance
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

FramesData ..- GenerateIntermediateMedia === GenerateSubtitles & GenerateVoice & GenerateAppearance ====== Compose --x FFmpegCompose

Subtitle1 & Subtitle2 & SubtitleRest & Voice1 & Voice2 & VoiceRest & Appearance1 & Appearance2 & AppearanceRest ..- FFmpegCompose
AssetFile1 & AssetFile2 & AssetFileRest ..- FFmpegCompose

Output(Output)
class Output Resource

FFmpegCompose .-> Output
:::
