module Path exposing
    ( Path
    , fromString
    , toString
    , Forks
    , toForks
    , betweenForks
    )

{-| 迷路の道順を表すデータ構造に関するモジュール。


# Path

@docs Path
@docs fromString
@docs toString


# Forks

@docs Forks
@docs toForks
@docs betweenForks

-}

import Set exposing (Set)



-- PATH


{-| 迷路の道順を表すデータ構造。
分かれ道で何番目の道を選ぶかの`List`。
-}
type alias Path =
    List Int


{-| Pathをコンマ区切りの文字列表現にする。

    toString [ 0, 1, 2 ] == "0,1,2"

-}
toString : Path -> String
toString path =
    List.map String.fromInt path |> String.join ","


combineMaybe : List (Maybe a) -> Maybe (List a)
combineMaybe =
    List.foldr (Maybe.map2 (::)) (Just [])


{-| コンマ区切りの文字列表現を変換可能な場合は`Maybe Path`に変換する。
整数でない文字列が混じったりして、変換不可能な場合は`Nothing`を返す。

    fromString "0,1,2" == Just [ 0, 1, 2 ]

    fromString "0,a,2" == Nothing

-}
fromString : String -> Maybe Path
fromString str =
    String.split "," str
        |> List.map String.toInt
        |> combineMaybe



-- FORKS


{-| 迷路の分かれ道を表すデータ構造。
スタートから分かれ道までのパスのなす`Set`。
-}
type alias Forks =
    Set Path


{-| Pathを分かれ道の集合へと変換する。

    toForks [ 0, 1, 2 ] == Set.fromList [ [], [ 0 ], [ 0, 1 ] ]

-}
toForks : Path -> Forks
toForks =
    throughList >> Set.fromList


{-| PathとPathの間の分かれ道の集合を得る。

    betweenForks [ 0 ] [ 0, 1, 2, 3 ] =
        Set.fromList [ [ 0, 1 ] [ 0, 1, 2 ] ]

-}
betweenForks : Path -> Path -> Forks
betweenForks startPath =
    throughList >> List.drop (List.length startPath) >> Set.fromList


throughList : Path -> List Path
throughList path =
    List.range 0 (List.length path - 1)
        |> List.map List.take
        |> List.map (\f -> f path)
