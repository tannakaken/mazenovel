module Route exposing (..)

import Url exposing (Url)
import Url.Parser as UP exposing ((</>), (<?>), Parser, s, top)
import Url.Parser.Query as Q


{-| seedは乱数のseedを生成するためのIntを、
pathは迷路の途中までの道順を表す。
-}
type alias Query =
    { seed : Maybe Int
    , path : Maybe String
    }


{-| TopはアプリのベースURLを表し、Queryの表すオプションのクエリを格納する。
-}
type Route
    = Top Query


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


urlToRoute : Url -> Maybe Route
urlToRoute url =
    UP.parse (routeParser url) url
