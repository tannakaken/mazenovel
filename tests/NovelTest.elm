module NovelTest exposing (..)

import Array exposing (Array)
import Expect exposing (Expectation)
import Novel exposing (NovelNode, NovelTree)
import Random
import Test exposing (..)


testNovelTree : NovelTree
testNovelTree =
    Array.fromList
        [ NovelNode (Just "h") (Array.fromList [ 1 ])
        , NovelNode (Just "e") (Array.fromList [ 2 ])
        , NovelNode (Just "l") (Array.fromList [ 3 ])
        , NovelNode (Just "l") (Array.fromList [ 4 ])
        , NovelNode (Just "o") (Array.fromList [])
        ]


suite : Test
suite =
    describe "The Novel Module"
        [ describe "Novel.randomNovel"
            [ test "make string from Novel.NovelTree" <|
                \_ ->
                    Expect.equal
                        (Novel.randomNovel (Random.initialSeed 0) testNovelTree)
                        ( "hello", [] )
            ]
        ]
