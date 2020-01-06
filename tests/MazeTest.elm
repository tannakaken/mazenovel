module MazeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Set

import Maze

suite : Test
suite =
    describe "The Maze module"
        [ describe "Maze.vonNeumannNeighborhood"
            [ test "return up down right left cell" <|
                \_ ->
                    let
                        neigborhood = Maze.vonNeumannNeighborhood (0,0)
                        expected = Set.fromList [(1, 0), (-1, 0), (0, 1), (0, -1)]
                    in
                    Expect.equal neigborhood expected
            ]
        ]
