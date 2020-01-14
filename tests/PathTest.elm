module PathTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Path
import Set
import Test exposing (..)


suite : Test
suite =
    describe "The Novel Module"
        [ describe "Path.toString"
            [ test "make String from Path" <|
                \_ ->
                    Expect.equal
                        (Path.toString [ 0, 2, 1 ])
                        "0,2,1"
            ]
        , describe "Path.fromString"
            [ test "make Path from String" <|
                \_ ->
                    Expect.equal
                        (Path.fromString "1,2,3")
                        (Just [ 1, 2, 3 ])
            , test "invalid string" <|
                \_ ->
                    Expect.equal
                        (Path.fromString "1,a,3")
                        Nothing
            ]
        , describe "Path.toForks"
            [ test "make Forks Set from Path" <|
                \_ ->
                    Expect.equal
                        (Path.toForks [ 1, 2, 3 ])
                        (Set.fromList [ [], [ 1 ], [ 1, 2 ] ])
            ]
        ]
