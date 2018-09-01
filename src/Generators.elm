module Generators exposing (Algorithm(..), getAlgorithm)

import List.Extra as List
import Random
import Types exposing (Room, Side(..))


type Algorithm
    = PlainGrid
    | BinaryTree
    | Sidewinder


type alias MazeGenerator =
    { seed : Random.Seed, width : Int, height : Int } -> List Room -> ( List Room, Random.Seed )


plainGridAlgorithm : MazeGenerator
plainGridAlgorithm { seed } rooms =
    ( rooms, seed )


randomBool : Random.Generator Bool
randomBool =
  Random.weighted (50, True) [ (50, False) ]

binaryTreeAlgorithm : MazeGenerator
binaryTreeAlgorithm { seed, width } rooms =
    let
        pickASide which =
            if which then
                Types.Right

            else
                Types.Top

        nextWalls fromSeed room =
            if (room.y == 0) && (room.x == width - 1) then
                ( Types.All, fromSeed )

            else if room.y == 0 then
                ( Types.Top, fromSeed )

            else if room.x == width - 1 then
                ( Types.Right, fromSeed )

            else
                Random.step (Random.map pickASide randomBool) fromSeed

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


sidewinderAlgorithm : MazeGenerator
sidewinderAlgorithm { seed, width, height } rooms =
    let
        getRow index indexRooms =
            List.filter (\{ x, y, walls } -> y == index) indexRooms

        sameRoom room1 room2 =
            room1.x == room2.x && room1.y == room2.y

        replaceRoom room ( updatedRooms, currentSeed ) =
            ( List.setIf (sameRoom room) room updatedRooms
            , currentSeed
            )

        replaceRooms index replRooms ( roomsToReplace, replaceSeed ) =
            List.foldl
                replaceRoom
                ( replRooms, replaceSeed )
                roomsToReplace

        updateTopRow updateWidth index ( updateRooms, updateSeed ) =
            ( List.map
                (\room ->
                    if room.x < updateWidth - 1 then
                        { room | walls = Top }

                    else
                        { room | walls = All }
                )
                rooms
            , seed
            )

        processRow processWidth processHeight index ( processRooms, processSeed ) =
            ( processRooms, processSeed )

        updateRow processor index ( updateRooms, updateSeed ) =
            ( getRow index updateRooms, updateSeed )
                |> processor index
                |> replaceRooms index updateRooms
    in
    updateRow (updateTopRow width) 0 ( rooms, seed )
        |> updateRow (processRow width height) 1


getAlgorithm : Algorithm -> MazeGenerator
getAlgorithm algorithm =
    case algorithm of
        PlainGrid ->
            plainGridAlgorithm

        BinaryTree ->
            binaryTreeAlgorithm

        Sidewinder ->
            sidewinderAlgorithm
