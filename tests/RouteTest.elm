module RouteTest exposing (..)

import Expect exposing (Expectation)
import Route exposing (..)
import Test exposing (..)
import Url


suite : Test
suite =
    describe "The Route module"
        [ describe "Route.urlToRoute"
            [ test "parse url" <|
                \_ ->
                    Url.fromString "http://example.org/"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query Nothing Nothing |> Top |> Just)
            , test "parse url with port" <|
                \_ ->
                    Url.fromString "http://example.org:8000/"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query Nothing Nothing |> Top |> Just)
            , test "parse url with path" <|
                \_ ->
                    Url.fromString "http://example.org/path/"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query Nothing Nothing |> Top |> Just)
            , test "parse url with query" <|
                \_ ->
                    Url.fromString "http://example.org/?seed=1"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query (Just 1) Nothing |> Top |> Just)
            , test "parse url with port & path" <|
                \_ ->
                    Url.fromString "http://example.org:8000/path/"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query Nothing Nothing |> Top |> Just)
            , test "parse url with port & query" <|
                \_ ->
                    Url.fromString "http://example.org:8000/?seed=1"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query (Just 1) Nothing |> Top |> Just)
            , test "parse url with path & query" <|
                \_ ->
                    Url.fromString "http://example.org/path/?seed=1"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query (Just 1) Nothing |> Top |> Just)
            , test "parse url with port, path & query" <|
                \_ ->
                    Url.fromString "http://example.org:8000/path/?seed=1"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query (Just 1) Nothing |> Top |> Just)
            , test "parse url with index.html" <|
                \_ ->
                    Url.fromString "http://example.org/index.html"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query Nothing Nothing |> Top |> Just)
            , test "parse url with index.html & path" <|
                \_ ->
                    Url.fromString "http://example.org/path/index.html"
                        |> Maybe.andThen Route.urlToRoute
                        |> Expect.equal
                            (Query Nothing Nothing |> Top |> Just)
            ]
        ]
