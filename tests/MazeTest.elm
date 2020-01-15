module MazeTest exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, tuple)
import Maze
import Set
import Test exposing (..)


testChooser : Maze.Chooser
testChooser =
    Maze.Chooser
        (\coordinatesset ->
            let
                coordinateslist =
                    Set.toList coordinatesset

                x =
                    List.head coordinateslist
            in
            case x of
                Nothing ->
                    Nothing

                Just coordinates ->
                    Just ( coordinates, testChooser )
        )


sampleMaze : List Maze.Coordinates -> Maze.Maze
sampleMaze coordinatess =
    case coordinatess of
        [] ->
            Dict.empty

        head :: rest ->
            Dict.insert head 'a' <| sampleMaze rest


suite : Test
suite =
    describe "The Maze module"
        [ describe "Maze.makeExit"
            [ test "make one coordinates path" <|
                \_ ->
                    Expect.equal
                        (Maze.makeExit testChooser "h")
                        (Dict.fromList [ ( ( 0, 0 ), 'h' ) ])
            , test "make two coordinates path" <|
                \_ ->
                    Expect.equal
                        (Maze.makeExit testChooser "he")
                        (Dict.fromList [ ( ( 0, 0 ), 'h' ), ( ( -1, 0 ), 'e' ) ])
            , test "make three coordinates path" <|
                \_ ->
                    Expect.equal
                        (Maze.makeExit testChooser "hel")
                        (Dict.fromList [ ( ( 0, 0 ), 'h' ), ( ( -1, 0 ), 'e' ), ( ( -2, 0 ), 'l' ) ])
            ]
        , describe "Maze.choiceOfNextCoordinates"
            [ test "choice of next coordinates" <|
                \_ ->
                    Expect.equal
                        (Maze.choiceOfNextCoordinates (sampleMaze [ ( 0, 0 ) ]) Set.empty ( 0, 0 ))
                        (Set.fromList [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ) ])
            , test "exceptions of next coordinates" <|
                \_ ->
                    Expect.equal
                        (Maze.choiceOfNextCoordinates (sampleMaze [ ( 0, 0 ) ]) (Set.fromList [ ( 0, 1 ) ]) ( 0, 0 ))
                        (Set.fromList [ ( 1, 0 ), ( -1, 0 ) ])
            ]
        , describe "Maze.canDig"
            [ test "can dig coordinates if it make new path and does not make new intersection" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ) ]) ( 0, 0 ) ( 0, 1 ) |> Expect.true "expected that can dig"
            , test "can not dig coordinates out of area" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ) ]) ( 0, 0 ) ( 0, -1 ) |> Expect.false "expected that can not dig"
            , test "can not dig coordinates in maze path" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ) ]) ( 1, 0 ) ( 0, 0 ) |> Expect.false "expected that can not dig"
            , test "can not dig coordinates adjacent to maze path" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]) ( 1, 1 ) ( 1, 0 ) |> Expect.false "expected that can not dig"
            ]
        ]
