namespace ImageMagick

open System.Diagnostics

type ImageMagick(path: string) =
    member __.Start(arguments: string) = Process.Start(path, arguments)
    member __.Start(arguments: string seq) = Process.Start(path, arguments)
