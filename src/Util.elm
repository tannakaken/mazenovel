module Util exposing (..)

import Url exposing (Url)


getNth : Int -> List a -> Maybe a
getNth nth list =
    list |> List.drop nth |> List.head



-- URL


baseUrl : Url -> String
baseUrl url =
    let
        scheme =
            case url.protocol of
                Url.Https ->
                    "https://"

                Url.Http ->
                    "http://"

        host =
            url.host

        portString =
            case url.port_ of
                Nothing ->
                    ""

                Just portNum ->
                    ":" ++ String.fromInt portNum
    in
    String.concat [ scheme, host, portString, String.replace "index.html" "" url.path ]


jsonUrl : Url -> String
jsonUrl url =
    baseUrl url ++ "tree.json"


seedUrl : Url -> Int -> String
seedUrl url seed =
    String.concat [ baseUrl url, "?seed=", String.fromInt seed ]
