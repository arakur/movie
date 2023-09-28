namespace Data

open Types

type Wav =
    { Bytes: byte[] }

    static member Create(bytes: byte[]) = { Bytes = bytes }

    member this.SaveAsync(path: Path) =
        System.IO.File.WriteAllBytesAsync(path, this.Bytes)

    static member saveAsync (this: Wav) (path: Path) = this.SaveAsync(path)
