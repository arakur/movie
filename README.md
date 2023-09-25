# [WIP] スクリプトによる Voicevox 動画生成ツール

## Setup

1. `dotnet`, `python`, `typst`, `magick`, `ffmpeg` の5つのコマンドに path を通す
   - それぞれ Windows 11 の以下のバージョンで動作確認済みです．

    ```nushell
    > dotnet --version
    7.0.306
    > python --version
    Python 3.10.4
    > typst --version
    typst 0.6.0 (cba200d4)
    > magick --version
    Version: ImageMagick 7.1.1-12 Q16-HDRI x64 a09d8dd:20230625 https://imagemagick.org
    Copyright: (C) 1999 ImageMagick Studio LLC
    License: https://imagemagick.org/script/license.php
    Features: Cipher DPC HDRI Modules OpenCL OpenMP(2.0)
    Delegates (built-in): bzlib cairo flif freetype gslib heic jng jp2 jpeg jxl lcms lqr lzma openexr pangocairo png ps raqm raw rsvg tiff webp xml zip zlib
    Compiler: Visual Studio 2022 (193632532)
    > ffmpeg -version
    ffmpeg version 6.0-full_build-www.gyan.dev Copyright (c) 2000-2023 the FFmpeg developers
    built with gcc 12.2.0 (Rev10, Built by MSYS2 project)
    configuration: --enable-gpl --enable-version3 --enable-shared --disable-w32threads --disable-autodetect --enable-fontconfig --enable-iconv --enable-gnutls --enable-libxml2 --enable-gmp --enable-bzlib --enable-lzma --enable-libsnappy --enable-zlib --enable-librist --enable-libsrt --enable-libssh --enable-libzmq --enable-avisynth --enable-libbluray --enable-libcaca --enable-sdl2 --enable-libaribb24 --enable-libdav1d --enable-libdavs2 --enable-libuavs3d --enable-libzvbi --enable-librav1e --enable-libsvtav1 --enable-libwebp --enable-libx264 --enable-libx265 --enable-libxavs2 --enable-libxvid --enable-libaom --enable-libjxl --enable-libopenjpeg --enable-libvpx --enable-mediafoundation --enable-libass --enable-frei0r --enable-libfreetype --enable-libfribidi --enable-liblensfun --enable-libvidstab --enable-libvmaf --enable-libzimg --enable-amf --enable-cuda-llvm --enable-cuvid --enable-ffnvcodec --enable-nvdec --enable-nvenc --enable-d3d11va --enable-dxva2 --enable-libvpl --enable-libshaderc --enable-vulkan --enable-libplacebo --enable-opencl --enable-libcdio --enable-libgme --enable-libmodplug --enable-libopenmpt --enable-libopencore-amrwb --enable-libmp3lame --enable-libshine --enable-libtheora --enable-libtwolame --enable-libvo-amrwbenc --enable-libilbc --enable-libgsm --enable-libopencore-amrnb --enable-libopus --enable-libspeex --enable-libvorbis --enable-ladspa --enable-libbs2b --enable-libflite --enable-libmysofa --enable-librubberband --enable-libsoxr --enable-chromaprint
    libavutil      58.  2.100 / 58.  2.100
    libavcodec     60.  3.100 / 60.  3.100
    libavformat    60.  3.100 / 60.  3.100
    libavdevice    60.  1.100 / 60.  1.100
    libavfilter     9.  3.100 /  9.  3.100
    libswscale      7.  1.100 /  7.  1.100
    libswresample   4. 10.100 /  4. 10.100
    libpostproc    57.  1.100 / 57.  1.100
    ```

2. 本リポジトリをクローン
3. `appearance/psd_resource` 下に `psd` ファイルを配置
   - 坂本アヒル氏の[四国めたん立ち絵素材2.1](https://www.pixiv.net/en/artworks/92641379)および[春日部つむぎ立ち絵素材2.0](https://www.pixiv.net/en/artworks/95429376)をダウンロードし，以下のように配置:

   ```plain
   appearance
    ├── psd_resource
    │   ├── 四国めたん立ち絵素材2.1
    │   │   ├── 四国めたん立ち絵素材2.1.psd
    │   ├── 春日部つむぎ立ち絵素材2.0
    │   │   ├── 春日部つむぎ立ち絵素材2.0.psd
   ```

4. リポジトリのルートディレクトリで以下を実行:

    ```nushell
    > python psd_extract.py appearance\psd_resources\四国めたん立ち絵素材2.1\四国めたん立ち絵素材2.1.psd 四国めたん appearance
    > python psd_extract.py appearance\psd_resources\春日部つむぎ立ち絵素材2.0\春日部 つむぎ立ち絵素材2.0.psd 春日部つむぎ appearance
    ```

5. [VOICEVOX CORE のリリースページ](https://github.com/VOICEVOX/voicevox_core/releases/tag/0.14.4)から zip ファイルをダウンロード，解凍し，中の `model` フォルダを `VoicevoxCore/bin` 下に配置

6. `dotnet run --project src/Example` を実行し，`output` フォルダ下に `output.mp4` が生成されることを確認
