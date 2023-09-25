namespace Appearance

type AppearanceError =
    | EmptyLocation
    | LayerNotFound of string list
    | LayerAlreadyTurnedOn of string list
    | LayerAlreadyTurnedOff of string list
    | LayerIsForced of string list
    | LayerIsRadioLayer of string list

    member this.Append name =
        match this with
        | EmptyLocation -> failwith "Internal error: tried to append a path segment to an empty location error"
        | LayerNotFound names -> LayerNotFound(name :: names)
        | LayerAlreadyTurnedOn names -> LayerAlreadyTurnedOn(name :: names)
        | LayerAlreadyTurnedOff names -> LayerAlreadyTurnedOff(name :: names)
        | LayerIsForced names -> LayerIsForced(name :: names)
        | LayerIsRadioLayer names -> LayerIsRadioLayer(name :: names)

    static member append name (error: AppearanceError) = error.Append name

type AppearanceResult<'a> = Result<'a, AppearanceError>
