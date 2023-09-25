namespace Data

type Wav =
    { Bytes: byte[] }

    static member Create(bytes: byte[]) = { Bytes = bytes }

    member this.Save(path: string) =
        System.IO.File.WriteAllBytes(path, this.Bytes)

    static member save (this: Wav) (path: string) = this.Save(path)
