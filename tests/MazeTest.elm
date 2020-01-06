module MazeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, tuple)
import Maze
import Set
import Test exposing (..)


suite : Test
suite =
    describe "The Maze module"
        [ describe "Maze.vonNeumannNeighborhood"
            [ fuzz (tuple ( int, int )) "return up dorn right left cell of arbitary point" <|
                \( x, y ) ->
                    let
                        neigborhood =
                            Maze.vonNeumannNeighborhood ( x, y )

                        expected =
                            Set.fromList [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
                    in
                    Expect.equal neigborhood expected
            ]
        ]
