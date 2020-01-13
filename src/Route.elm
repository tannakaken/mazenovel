module Route exposing (..)

import Url exposing (Url)
import Url.Parser as UP exposing ((</>), (<?>), Parser, s, top)
import Url.Parser.Query as Q


type alias Query =
    { seed : Maybe Int
    , path : Maybe String
    }


type Route
    = Top Query


pathParser : Url -> Parser a a
pathParser url =
    String.split "/" url.path
        |> List.filter (\x -> String.isEmpty x |> not)
        |> List.map s
        |> List.foldl (</>) top


routeParser : Url -> Parser (Route -> a) a
routeParser url =
    UP.oneOf
        [ UP.map Top (pathParser url <?> Q.map2 Query (Q.int "seed") (Q.string "path")) ]


urlToRoute : Url -> Maybe Route
urlToRoute url =
    UP.parse (routeParser url) url
