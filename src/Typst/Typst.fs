namespace Typst

open Measure

type Weight =
    | Thin
    | ExtraLight
    | Light
    | Regular
    | Medium
    | SemiBold
    | Bold
    | ExtraBold
    | Black
    | Weight of int

    member this.Compose() =
        match this with
        | Thin -> "\"thin\""
        | ExtraLight -> "\"extra-light\""
        | Light -> "\"light\""
        | Regular -> "\"regular\""
        | Medium -> "\"medium\""
        | SemiBold -> "\"semi-bold\""
        | Bold -> "\"bold\""
        | ExtraBold -> "\"extra-bold\""
        | Black -> "\"black\""
        | Weight i -> sprintf "%d" i

type Color =
    | RGB of int * int * int
    | RGBA of int * int * int * int

    member this.Compose() =
        match this with
        | RGB(r, g, b) -> sprintf "rgb(%d, %d, %d)" r g b
        | RGBA(r, g, b, a) -> sprintf "rgba(%d, %d, %d, %d)" r g b a

type Page =
    { Width: float<pt>
      Height: float<pt>
      Margin: float<pt> }

type Text =
    { Size: float<pt>
      Weight: Weight
      Fill: Color }

type TypstSource =
    { Page: Page
      Text: Text
      Content: string }

    member this.Compose() =
        let page =
            sprintf
                "#set page(width: %fpt, height: %fpt, margin: %fpt)"
                this.Page.Width
                this.Page.Height
                this.Page.Margin

        let text =
            sprintf
                "#set text(size: %fpt, weight: \"%s\", fill: %s)"
                this.Text.Size
                (this.Text.Weight.Compose())
                (this.Text.Fill.Compose())

        sprintf "%s\n%s\n%s" page text this.Content

type Typst(path: string) =
    member __.CompileAsync(src: TypstSource, outPath: string) =
        let uuid = System.Guid.NewGuid().ToString("N")

        let srcPath = sprintf "tmp/typst_src_%s.typ" uuid

        task {
            do! System.IO.File.WriteAllTextAsync(srcPath, src.Compose())

            do!
                System.Diagnostics.Process
                    .Start(path, sprintf "compile %s %s" srcPath outPath)
                    .WaitForExitAsync()

            do System.IO.File.Delete(srcPath)
        }
