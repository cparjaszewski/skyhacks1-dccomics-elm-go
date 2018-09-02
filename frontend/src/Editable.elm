module Editable exposing (Editable(..), bufferValue, cancelEditing, commitBuffer, finishEditing, hasChanged, isEditing, newEditing, setBuffer, startEditing, setValue)


type Editable ofType
    = NotEditing { value : ofType }
    | Editing { originalValue : ofType, buffer : ofType }


setValue : Editable ofType -> ofType
setValue editable =
    case editable of
        NotEditing { value } ->
            value

        Editing { originalValue } ->
            originalValue


bufferValue : Editable ofType -> ofType
bufferValue editable =
    case editable of
        NotEditing { value } ->
            value

        Editing { buffer } ->
            buffer


commitBuffer : Editable ofType -> Editable ofType
commitBuffer editable =
    case editable of
        NotEditing _ ->
            editable

        Editing { buffer } ->
            Editing { originalValue = buffer, buffer = buffer }


setBuffer : Editable ofType -> ofType -> Editable ofType
setBuffer editable newBuffer =
    case editable of
        NotEditing _ ->
            editable

        Editing values ->
            Editing { values | buffer = newBuffer }


newEditing : ofType -> Editable ofType
newEditing newEditingValue =
    Editing { originalValue = newEditingValue, buffer = newEditingValue }


startEditing : Editable ofType -> Editable ofType
startEditing editable =
    case editable of
        NotEditing { value } ->
            setBuffer editable value

        _ ->
            editable


finishEditing : Editable ofType -> Editable ofType
finishEditing editable =
    case editable of
        NotEditing _ ->
            editable

        Editing { buffer } ->
            NotEditing { value = buffer }


cancelEditing : Editable ofType -> Editable ofType
cancelEditing editable =
    case editable of
        NotEditing _ ->
            editable

        Editing { originalValue } ->
            NotEditing { value = originalValue }


isEditing : Editable ofType -> Bool
isEditing editable =
    case editable of
        NotEditing _ ->
            False

        Editing _ ->
            True


hasChanged : Editable comparable -> Bool
hasChanged editable =
    case editable of
        NotEditing _ ->
            False

        Editing { originalValue, buffer } ->
            originalValue /= buffer
