module Main exposing (Dimension(..), Model, Msg(..), coordinates, generateMaze, generateRooms, init, main, mazeHeight, mazeView, mazeWidth, ourPostRequest, roomView, roomsView, scale, scaledSizeInPx, selectionForm, someDecoder, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Debug exposing (log, toString)
import Editable exposing (Editable, newEditing, setValue)
import Generators exposing (Algorithm(..))
import Html exposing (Html, button, div, input, label, span, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http exposing (expectStringResponse, jsonBody, post)
import Json.Decode exposing (Decoder, Value, decodeString, field, int, list, string)
import Json.Encode as Encode
import List.Extra as List
import Random
import Types exposing (Room, Side)
import Url exposing (Url)

someDecoder : Decoder String
someDecoder = field "cokolwiek" string

type alias Model =
    { width : Editable Int
    , height : Editable Int
    , seedForSideGenerator : Random.Seed
    , rooms : List Room
    , algorithm : Algorithm
    , backendIndicator : Int
    }

type Dimension = Width | Height

type Msg
    = SetSideSeed Random.Seed
    | SetDimension Dimension String
    | CommitDimensionChanges
    | ChooseAlgorithm Algorithm
    | RegenerateMaze
    | SaveMazeClick
    | SaveMaze (Result Http.Error String)
    | None

scale = 10

mazeWidth : Model -> Int
mazeWidth model = Editable.setValue model.width

mazeHeight : Model -> Int
mazeHeight model =
    Editable.setValue model.height

init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navkey=
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
            , algorithm = Sidewinder
            , backendIndicator = 0
            }

        generateInitialSideSeedCmd =
            Random.int Random.minInt Random.maxInt
                |> Random.map Random.initialSeed
                |> Random.generate SetSideSeed
    in
    ( initialModel
    , generateInitialSideSeedCmd
    )

coordinates : Int -> Int -> List ( Int, Int )
coordinates width height =
    List.lift2 (\a b -> ( a, b )) (List.range 0 (width - 1)) (List.range 0 (height - 1))

generateRooms : Model -> List Room
generateRooms model =
    coordinates (mazeWidth model) (mazeHeight model)
        |> List.map (\( x, y ) -> { x = x, y = y, walls = Types.All })

generateMaze : Algorithm -> Model -> Model
generateMaze algorithm model =
    let
        mazeGenerator =
            Generators.getAlgorithm algorithm

        ( rooms, finalSeed ) =
            generateRooms model
                |> mazeGenerator { seed = model.seedForSideGenerator, width = mazeWidth model, height = mazeHeight model }
    in
    { model | algorithm = algorithm, rooms = rooms, seedForSideGenerator = finalSeed }

ourPostRequest : Model -> Http.Request String
ourPostRequest model =
    let
        jsonModelBody =
            [ ( "backendIndicator", Encode.int model.backendIndicator ) ]
                |> Encode.object
                |> Http.jsonBody
                |> log "ourPostRequest"

        url =
            "https://onet.pl/save"
    in
    Http.request
        { body = jsonModelBody
        , expect = Http.expectJson someDecoder
        , headers = [Http.header "Content-type" "application/json", Http.header "charset" "utf-8" ]
        , method = "POST"
        , timeout = Nothing
        , url = url
        , withCredentials = False
        }
        |> log "request"

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SaveMazeClick ->
            ( model
            , Http.send SaveMaze (ourPostRequest model)
            )

        SaveMaze result ->
            let
                updatedBackendIndicator =
                    case result of
                        Ok _ ->
                            1

                        Err _ ->
                            2
            in
            ( { model | backendIndicator = updatedBackendIndicator }
            , Cmd.none
            )

        RegenerateMaze ->
            let
                updatedModel =
                    generateMaze model.algorithm model
            in
            ( updatedModel
            , Cmd.none
            )

        ChooseAlgorithm algorithm ->
            let
                updatedModel =
                    generateMaze algorithm model
            in
            ( updatedModel
            , Cmd.none
            )

        SetSideSeed seed ->
            let
                updatedModel =
                    generateMaze model.algorithm { model | seedForSideGenerator = seed }
            in
            ( updatedModel
            , Cmd.none
            )

        CommitDimensionChanges ->
            let
                updatedModel =
                    generateMaze model.algorithm { model | width = Editable.commitBuffer model.width, height = Editable.commitBuffer model.height }
            in
            ( updatedModel
            , Cmd.none
            )

        SetDimension which newValue ->
            let
                updateProperty property buffer =
                    Editable.setBuffer property buffer

                newValueInt = (Result.withDefault 0 (Result.fromMaybe ("Error parsing newValue" ++ toString newValue) (String.toInt newValue) ))

                updatedModel =
                    case which of
                        Height ->
                            { model | height = updateProperty model.height newValueInt }

                        Width ->
                            { model | width = updateProperty model.width newValueInt }
            in
            ( updatedModel
            , Cmd.none
            )

        None ->
             ( model
             , Cmd.none
             )

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
                Types.Right ->
                    ( "0px", "1px solid black" )

                Types.Top ->
                    ( "1px solid black", "0px" )

                Types.All ->
                    ( "1px solid black", "1px solid black" )
    in
    div
        [ style "position" "absolute"
        , style "border-top" topBorder
        , style "border-right" rightBorder
        , style "padding" "0"
        , style "margin" "0"
        , style "left" left
        , style "top" top
        , style "width" width
        , style "height" width
        ]
        []

roomsView : Model -> List (Html Msg)
roomsView model =
    List.map roomView model.rooms

scaledSizeInPx : Int -> String
scaledSizeInPx size =
    toString (size * scale) ++ "px"

mazeView : Model -> Html Msg
mazeView model =
    let
        width =
            scaledSizeInPx <| mazeWidth model

        height =
            scaledSizeInPx <| mazeHeight model
    in
    div [ style "position" "relative", style "border-bottom" "1px solid black", style "border-left" "1px solid black", style "width" width, style "height" height, style "margin-left" "20px", style "margin-top" "20px" ] <|
        roomsView model

selectionForm : Model -> Html Msg
selectionForm model =
    let
        algorithmChooser algorithm currentAlgorithm label =
            if algorithm == currentAlgorithm then
                span [] [ text <| "<X> " ++ label ]

            else
                span [ onClick <| ChooseAlgorithm algorithm ] [ text <| "< > " ++ label ]
    in
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
        , div []
            [ text "Algorithm"
            , span [] [ text " | " ]
            , algorithmChooser PlainGrid model.algorithm "None"
            , span [] [ text " | " ]
            , algorithmChooser BinaryTree model.algorithm "Binary Tree"
            , span [] [ text " | " ]
            , algorithmChooser Sidewinder model.algorithm "Sidewinder"
            ]
        , button [ onClick RegenerateMaze ] [ text "Regenerate Maze" ]
        , button [ onClick SaveMazeClick ] [ text "Save Maze" ]
        , text (toString model.backendIndicator)
        ]

view : Model -> {
    title : String
    , body : List (Html Msg)
    }
view model =
    {
        title = "Elm & Go Hackathon"
        , body = [
            div []
                [ selectionForm model
                , mazeView model
                ]
        ]
    }

type alias Document msg =
  { title : String
  , body : List (Html msg)
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

application :
        { init :  flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
        , onUrlChange : Url -> Msg
        , onUrlRequest : Browser.UrlRequest -> Msg
        , subscriptions : Model -> Sub Msg
        , update : Msg -> Model -> ( Model, Cmd Msg )
        , view : Model -> Browser.Document Msg
        }
    -> Program Value Model Msg
application config =
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }

main : Program Value Model Msg
main =
    application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = (\_ -> None)
        , onUrlRequest = (\_ -> None)
        }
