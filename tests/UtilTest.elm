module UtilTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Url
import Util


suite : Test
suite =
    describe "The Util module"
        [ describe "Util.getNth"
            [ test "get nth element of list" <|
                \_ ->
                    Expect.equal
                        (Util.getNth 0 [ 1, 2, 4, 8, 16 ])
                        (Just 1)
            , test "get last element of list" <|
                \_ ->
                    Expect.equal
                        (Util.getNth 4 [ 1, 2, 4, 8, 16 ])
                        (Just 16)
            , test "can not get out of range" <|
                \_ ->
                    Expect.equal
                        (Util.getNth 5 [ 1, 2, 4, 8, 16 ])
                        Nothing
            , test "can not get minus index" <|
                \_ ->
                    Expect.equal
                        (Util.getNth -1 [ 1, 2, 4, 8, 16 ])
                        (Just 1)
            ]
        , describe "Util.jsonUrl"
            [ test "get json url from local server" <|
                \_ ->
                    Url.fromString "http://localhost:8000"
                        |> Maybe.map Util.jsonUrl
                        |> Expect.equal (Just "http://localhost:8000/tree.json")
            , test "get json url from local server with index.html" <|
                \_ ->
                    Url.fromString "https://localhost:8000/index.html"
                        |> Maybe.map Util.jsonUrl
                        |> Expect.equal (Just "https://localhost:8000/tree.json")
            , test "get json url from global server" <|
                \_ ->
                    Url.fromString "https://tannakaken.xyz/mazenovel/"
                        |> Maybe.map Util.jsonUrl
                        |> Expect.equal (Just "https://tannakaken.xyz/mazenovel/tree.json")
            , test "get json url from global server with index.html" <|
                \_ ->
                    Url.fromString "https://tannakaken.xyz/mazenovel/index.html"
                        |> Maybe.map Util.jsonUrl
                        |> Expect.equal (Just "https://tannakaken.xyz/mazenovel/tree.json")
            ]
        , describe "Util.seedUrl"
            [ test "get seed url from local server" <|
                \_ ->
                    Url.fromString "http://localhost:8000"
                        |> Maybe.map (Util.seedUrl 1)
                        |> Expect.equal (Just "http://localhost:8000/?seed=1")
            , test "get seed url from local server with index.html" <|
                \_ ->
                    Url.fromString "https://localhost:8000/index.html"
                        |> Maybe.map (Util.seedUrl 2)
                        |> Expect.equal (Just "https://localhost:8000/?seed=2")
            , test "get seed url from global server" <|
                \_ ->
                    Url.fromString "http://tannakaken.xyz/mazenovel/"
                        |> Maybe.map (Util.seedUrl 10)
                        |> Expect.equal (Just "http://tannakaken.xyz/mazenovel/?seed=10")
            , test "get seed url from global server with index.html" <|
                \_ ->
                    Url.fromString "https://tannakaken.xyz/mazenovel/index.html"
                        |> Maybe.map (Util.seedUrl 100)
                        |> Expect.equal (Just "https://tannakaken.xyz/mazenovel/?seed=100")
            ]
        ]
