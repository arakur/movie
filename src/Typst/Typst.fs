﻿namespace Typst

open Measure
open Types

[<RequireQualifiedAccess>]
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
    | OfInt of int

    static member tryFrom s =
        match s with
        | "thin" -> Some Thin
        | "extra-light" -> Some ExtraLight
        | "light" -> Some Light
        | "regular" -> Some Regular
        | "medium" -> Some Medium
        | "semi-bold" -> Some SemiBold
        | "bold" -> Some Bold
        | "extra-bold" -> Some ExtraBold
        | "black" -> Some Black
        | _ -> None

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
        | OfInt i -> sprintf "%d" i

type Page =
    { Width: float<pt>
      Height: float<pt>
      Margin: float<pt> }

type Text =
    { Size: float<pt> option
      Weight: Weight option
      Fill: Color option }

type TypstSource =
    { Page: Page
      FontFamily: string list
      Text: Text
      Content: string }

    member this.Compose() =
        // TODO: Enable to set font.
        let page =
            sprintf
                "#set page(width: %fpt, height: %fpt, margin: %fpt)"
                this.Page.Width
                this.Page.Height
                this.Page.Margin

        let size = this.Text.Size |> Option.map (sprintf "size: %fpt")
        let weight = this.Text.Weight |> Option.map (fun w -> $"weight: {w.Compose()}")
        let fill = this.Text.Fill |> Option.map (fun c -> $"fill: {c.Compose()}")

        let font =
            if this.FontFamily = [] then
                None
            else
                let content = this.FontFamily |> Seq.map (sprintf "\"%s\"") |> String.concat " "
                Some $"font: {content}"

        let text =
            [ size; weight; fill; font ]
            |> List.choose id
            |> String.concat ", "
            |> sprintf "#set text(%s)"

        let content = this.Content.Replace("\n", "\\\n")

        sprintf "%s\n%s\n%s" page text content

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

        // do System.IO.File.Delete(srcPath) // DEBUG
        }
