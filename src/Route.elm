module Route exposing (Query, Route(..), urlToRoute)

{-| 受け取ったUrlを処理するモジュール。


# Route

@docs Query, Route, urlToRoute

-}

import Url exposing (Url)
import Url.Parser as UP exposing ((</>), (<?>), Parser, s, top)
import Url.Parser.Query as Q


{-| seedは乱数のseedを生成するための`Int`を、
pathは迷路の途中までの道順を表す`"0,1,0"のような自然数のコンマ区切りの`String\`を表す。
-}
type alias Query =
    { seed : Maybe Int
    , path : Maybe String
    }


{-| TopはアプリのベースURLを表し、Queryの表すオプションのクエリを格納する。
-}
type Route
    = Top Query


{-| 邪魔なのでindex.htmlを除く。
-}
removeIndexHtml : Url -> Maybe Url
removeIndexHtml =
    Url.toString >> String.replace "index.html" "" >> Url.fromString


{-| <http://localhost:8000のようなURLでもhttps://tannakaken.xyz/mazenovelのようなパス部分のあるURLでも、>
どちらでも設定なしで使えるようにするための関数。
-}
pathParser : Url -> Parser a a
pathParser url =
    String.split "/" url.path
        |> List.filter (\x -> String.isEmpty x |> not)
        |> List.map s
        |> List.foldl (</>) top


{-| パス部分を無視して、seedとpathの二つのクエリを受け取る。
-}
routeParser : Url -> Parser (Route -> a) a
routeParser url =
    UP.oneOf
        [ UP.map Top (pathParser url <?> Q.map2 Query (Q.int "seed") (Q.string "path")) ]


{-| URLを受け取って、パースする関数。

    Maybe.andThen urlToRoute
        (Url.fromString "http://example.org")
        == Just (Top (Query Nothing Nothing))

    Maybe.andThen urlToRoute
        Url.fromString
        "http://example.org/path/?seed=1"
        == Just (Top (Query (Just 1) Nothing))

    Maybe.andThen urlToRoute
        Url.fromString
        "http://example.org/?path=0,1,0"
        == Just (Top (Query Nothing (Just "0,1,0")))

    Maybe.andThen urlToRoute
        (Url.fromString "http://example.org/?seed=1&path=0,0,1")
        == Just (Top (Query (Just 1) (Just "0.0.1")))

-}
urlToRoute : Url -> Maybe Route
urlToRoute =
    removeIndexHtml >> Maybe.andThen urlToRouteAux


{-| -}
urlToRouteAux : Url -> Maybe Route
urlToRouteAux url =
    UP.parse (routeParser url) url
