module Util exposing (..)

import Url exposing (Url)


{-| Listのn番目の要素を取得する。
O(n)の時間がかかるので長いリストに対しては使わないこと。
長いリストに何回も使いたければArrayに変換し、O(1)のArray.getを使う。
また、負のインデックスの要素を取得しようとすると、0番目の要素を返すことにも注意。
-}
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


seedUrl : Int -> Url -> String
seedUrl seed url =
    String.concat [ baseUrl url, "?seed=", String.fromInt seed ]
