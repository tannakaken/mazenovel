module Util exposing (getNth, baseUrl, jsonUrl, seedUrl)

{-| Utility関数モジュール。


# Utility

@docs getNth, baseUrl, jsonUrl, seedUrl

-}

import Url exposing (Url)



-- UTILITY


{-| Listのn番目の要素を取得する。
O(n)の時間がかかるので長いリストに対しては使わないこと。
長いリストに何回も使いたければArrayに変換し、O(1)のArray.getを使う。
また、負のインデックスの要素を取得しようとすると、0番目の要素を返すことにも注意。

    Util.getNth 0 [ 0, 1, 2 ] == Just 0

    Util.getNth 1 [ 0, 1, 2 ] == Just 1

    Util.getNth 2 [ 0, 1, 2 ] == Just 2

    Util.getNth 3 [ 0, 1, 2 ] == Nothing

    Util.getNth -1 [ 0, 1, 2 ] == Just 0 -- !

-}
getNth : Int -> List a -> Maybe a
getNth nth list =
    list |> List.drop nth |> List.head



-- URL


{-| アプリのベースURL。

    base.Url (Url.fromString "https://tannakaken.xyz/mazenovel/index.html") == "https://tannakaken.xyz/mazenovel/"

-}
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


{-| 迷路小説データを格納したjsonファイルのURL。

    jsonUrl
        (Url.fromString
            "https://tannakaken.xyz/mazenovel/index.html"
        )
        == "https://tannakaken.xyz/mazenovel/tree.json"

-}
jsonUrl : Url -> String
jsonUrl url =
    baseUrl url ++ "tree.json"


{-| 乱数のseedをクエリで指定したURL。

    seedUrl 1
        (Url.fromString
            "https://tannakaken.xyz/mazenovel/index.html"
        )
        == "https://tannakaken.xyz/mazenovel/?seed=1"

-}
seedUrl : Int -> Url -> String
seedUrl seed url =
    String.concat [ baseUrl url, "?seed=", String.fromInt seed ]
