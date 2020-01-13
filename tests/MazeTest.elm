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
        (\cellset ->
            let
                celllist =
                    Set.toList cellset

                x =
                    List.head celllist
            in
            case x of
                Nothing ->
                    Nothing

                Just cell ->
                    Just ( cell, testChooser )
        )


sampleMaze : List Maze.Cell -> Maze.Maze
sampleMaze cells =
    case cells of
        [] ->
            Dict.empty

        head :: rest ->
            Dict.insert head "a" <| sampleMaze rest


suite : Test
suite =
    describe "The Maze module"
        [ describe "Maze.novelPath"
            [ test "make one cell path" <|
                \_ ->
                    Expect.equal
                        (Maze.novelPath testChooser "h")
                        (Dict.fromList [ ( ( 0, 0 ), "h" ) ])
            , test "make two cell path" <|
                \_ ->
                    Expect.equal
                        (Maze.novelPath testChooser "he")
                        (Dict.fromList [ ( ( 0, 0 ), "h" ), ( ( -1, 0 ), "e" ) ])
            , test "make three cell path" <|
                \_ ->
                    Expect.equal
                        (Maze.novelPath testChooser "hel")
                        (Dict.fromList [ ( ( 0, 0 ), "h" ), ( ( -1, 0 ), "e" ), ( ( -2, 0 ), "l" ) ])
            ]
        , describe "Maze.choiceOfNextCell"
            [ test "choice of next cell" <|
                \_ ->
                    Expect.equal
                        (Maze.choiceOfNextCell (sampleMaze [ ( 0, 0 ) ]) Set.empty ( 0, 0 ))
                        (Set.fromList [ ( 1, 0 ), ( -1, 0 ), ( 0, 1 ) ])
            , test "exceptions of next cell" <|
                \_ ->
                    Expect.equal
                        (Maze.choiceOfNextCell (sampleMaze [ ( 0, 0 ) ]) (Set.fromList [ ( 0, 1 ) ]) ( 0, 0 ))
                        (Set.fromList [ ( 1, 0 ), ( -1, 0 ) ])
            ]
        , describe "Maze.canDig"
            [ test "can dig cell if it make new path and does not make new intersection" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ) ]) ( 0, 0 ) ( 0, 1 ) |> Expect.true "expected that can dig"
            , test "can not dig cell out of area" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ) ]) ( 0, 0 ) ( 0, -1 ) |> Expect.false "expected that can not dig"
            , test "can not dig cell in maze path" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ) ]) ( 1, 0 ) ( 0, 0 ) |> Expect.false "expected that can not dig"
            , test "can not dig cell adjacent to maze path" <|
                \_ ->
                    Maze.canDig (sampleMaze [ ( 0, 0 ), ( 0, 1 ), ( 1, 1 ) ]) ( 1, 1 ) ( 1, 0 ) |> Expect.false "expected that can not dig"
            ]
        ]
