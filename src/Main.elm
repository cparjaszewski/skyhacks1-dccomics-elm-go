module Main exposing (..)

import Editable exposing (Editable)
import Html exposing (Html, text, div, input, label, button)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onInput, onClick)
import Random
import List.Extra as List


type alias Model =
    { width : Editable Int
    , height : Editable Int
    , seedForSideGenerator : Random.Seed
    , rooms : List Room
    }


type alias Room =
    { x : Int
    , y : Int
    , walls : Side
    }


type Side
    = All
    | Right
    | Top


type alias Sides =
    List Side


type Dimension
    = Width
    | Height


type Msg
    = SetSideSeed Random.Seed
    | SetDimension Dimension String
    | CommitDimensionChanges


scale =
    10


mazeWidth : Model -> Int
mazeWidth model =
    Editable.value model.width


mazeHeight : Model -> Int
mazeHeight model =
    Editable.value model.height


coordinates : Int -> Int -> List ( Int, Int )
coordinates width height =
    List.lift2 (,) (List.range 0 (width - 1)) (List.range 0 (height - 1))


generateRooms : Model -> List Room
generateRooms model =
    coordinates (mazeWidth model) (mazeHeight model)
        |> List.map (\( x, y ) -> { x = x, y = y, walls = All })


init : ( Model, Cmd Msg )
init =
    let
        initialWidth =
            5

        initialHeight =
            5

        initialModel =
            { width = Editable.newEditing initialWidth
            , height = Editable.newEditing initialHeight
            , seedForSideGenerator = Random.initialSeed 0
            , rooms = []
            }

        generateInitialSideSeedCmd =
            Random.int Random.minInt Random.maxInt
                |> Random.map Random.initialSeed
                |> Random.generate SetSideSeed
    in
        ( initialModel
        , generateInitialSideSeedCmd
        )


binaryTreeAlgorithm : { seed : Random.Seed, width : Int, height : Int } -> List Room -> ( List Room, Random.Seed )
binaryTreeAlgorithm { seed, width } rooms =
    let
        pickASide which =
            if which then
                Right
            else
                Top

        nextWalls fromSeed room =
            if (room.y == 0) && (room.x == width - 1) then
                ( All, fromSeed )
            else if room.y == 0 then
                ( Top, fromSeed )
            else if room.x == width - 1 then
                ( Right, fromSeed )
            else
                Random.step (Random.map pickASide Random.bool) fromSeed

        figureOutSide room ( updatedRooms, currentSeed ) =
            let
                ( walls, nextSeed ) =
                    nextWalls currentSeed room
            in
                ( { room | walls = walls } :: updatedRooms
                , nextSeed
                )
    in
        List.foldl
            figureOutSide
            ( [], seed )
            rooms


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSideSeed seed ->
            let
                ( rooms, finalSeed ) =
                    generateRooms model
                        |> binaryTreeAlgorithm { seed = seed, width = mazeWidth model, height = mazeHeight model }
            in
                ( { model | rooms = rooms, seedForSideGenerator = finalSeed }
                , Cmd.none
                )

        SetDimension Width newWidth ->
            let
                updatedWidth =
                    case String.toInt newWidth of
                        Ok w ->
                            Editable.setBuffer model.width w

                        Err _ ->
                            model.width
            in
                ( { model | width = updatedWidth }
                , Cmd.none
                )

        SetDimension Height newValue ->
            let
                updatedHeight =
                    case String.toInt newValue of
                        Ok w ->
                            Editable.setBuffer model.height w

                        Err _ ->
                            model.height
            in
                ( { model | height = updatedHeight }
                , Cmd.none
                )

        CommitDimensionChanges ->
            let
                updatedModel =
                    { model | width = Editable.commitBuffer model.width, height = Editable.commitBuffer model.height }

                ( rooms, finalSeed ) =
                    generateRooms updatedModel
                        |> binaryTreeAlgorithm { seed = model.seedForSideGenerator, width = mazeWidth updatedModel, height = mazeHeight updatedModel }
            in
                ( { updatedModel | rooms = rooms, seedForSideGenerator = finalSeed }
                , Cmd.none
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


roomView : Room -> Html Msg
roomView { x, y, walls } =
    let
        left =
            scaledSizeInPx x

        top =
            scaledSizeInPx y

        width =
            scaledSizeInPx 1

        ( topBorder, rightBorder ) =
            case walls of
                Right ->
                    ( "0px", "1px solid black" )

                Top ->
                    ( "1px solid black", "0px" )

                All ->
                    ( "1px solid black", "1px solid black" )
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "border-top", topBorder )
                , ( "border-right", rightBorder )
                , ( "padding", "0" )
                , ( "margin", "0" )
                , ( "left", left )
                , ( "top", top )
                , ( "width", width )
                , ( "height", width )
                ]
            ]
            []


roomsView : Model -> List (Html Msg)
roomsView model =
    List.map roomView model.rooms


scaledSizeInPx : Int -> String
scaledSizeInPx size =
    (toString (size * scale)) ++ "px"


mazeView : Model -> Html Msg
mazeView model =
    let
        width =
            scaledSizeInPx <| mazeWidth model

        height =
            scaledSizeInPx <| mazeHeight model
    in
        div [ style [ ( "position", "relative" ), ( "border-bottom", "1px solid black" ), ( "border-left", "1px solid black" ), ( "width", width ), ( "height", height ), ( "margin-left", "20px" ), ( "margin-top", "20px" ) ] ] <|
            roomsView model


selectionForm : Model -> Html Msg
selectionForm model =
    div []
        [ div []
            [ label [] [ text "Width" ]
            , input [ type_ "number", onInput (SetDimension Width), value <| toString <| Editable.bufferValue model.width ] []
            ]
        , div []
            [ label [] [ text "Height" ]
            , input [ type_ "number", onInput (SetDimension Height), value <| toString <| Editable.bufferValue model.height ] []
            ]
        , button [ onClick CommitDimensionChanges ] [ text "Set New Dimensions" ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ selectionForm model
        , mazeView model
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
