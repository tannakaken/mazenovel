module NovelTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Novel
import Random
import Set
import Test exposing (..)


testNovelTree : Novel.Tree
testNovelTree =
    Array.fromList
        [ Novel.Node (Just "h") (Array.fromList [ 1 ])
        , Novel.Node (Just "e") (Array.fromList [ 2 ])
        , Novel.Node (Just "l") (Array.fromList [ 3 ])
        , Novel.Node (Just "l") (Array.fromList [ 4 ])
        , Novel.Node (Just "o") (Array.fromList [])
        ]


suite : Test
suite =
    describe "The Novel Module"
        [ describe "Novel.select"
            [ test "make string from Novel.NovelTree" <|
                \_ ->
                    Expect.equal
                        (Novel.select (Random.initialSeed 0) [] testNovelTree)
                        (Just ( "hello", [] ))
            ]
        ]
