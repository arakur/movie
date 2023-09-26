namespace Data

type Wav =
    { Bytes: byte[] }

    static member Create(bytes: byte[]) = { Bytes = bytes }

    member this.SaveAsync(path: string) =
        System.IO.File.WriteAllBytesAsync(path, this.Bytes)

    static member saveAsync (this: Wav) (path: string) = this.SaveAsync(path)
