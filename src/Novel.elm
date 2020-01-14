module Novel exposing
    ( Node
    , Tree
    , select
    )

{-| 木構造になった小説のデータ構造に関するモジュール。


# Novel

@docs Node
@docs Tree


# Select

@docs select

-}

import Array exposing (Array)
import Path exposing (Path)
import Random
import Set exposing (Set)



-- NOVEL


{-| nodeは小説の一文字を、nextは次の文字を格納した`Node`の`Tree`内でのindexを表す。
-}
type alias Node =
    { node : Maybe String
    , next : Array Int
    }


{-| 途中から分岐する小説を表す、木構造のデータ構造。
自裁にはこのデータ構造は木構造ではなくループの存在する一般のグラフや存在しないノードへの接続を許してしまっている。
再帰的データ構造にすると、jsonをパースして読み込む際にstackが溢れたのでこのようなデータ構造にした。

    Array.fromList
        [ { node = Just "h", next = Array.fromList [ 1, 2 ] }
        , { node = Just "e", next = Array.fromList [ 3 ] }
        , { node = Just "o", next = Array.fromList [ 4 ] }
        , { node = Just "l", next = Array.fromList [ 5 ] }
        , { node = Just "l", next = Array.fromList [ 6 ] }
        , { node = Just "l", next = Array.fromList [ 7, 8 ] }
        , { node = Just "a", next = Array.empty }
        , { node = Just "o", next = Array.empty }
        , { node = Nothing, next = Array.empty }
        ]

によって

    h - e - l - l - 0
      \           \
        l - l - a

という`"hello"`と`"hell"`と`"hola"`の三つの文字列を併合した木構造を表現できる。

-}
type alias Tree =
    Array Node



-- SELECT


{-| Pathに従ってNovelTreeを辿り、Pathが途切れたあとは、ランダムに選ぶ。
最終的に生成された小説と、辿ってきたNovelPathを返す。
Pathが不正で、そのような道が存在しない時はNothingを返す。
`Tree`における例を`tree`とし、適当な`Random.Seed`型の`seed`を与えれば、

    select seed [] tree == Just ( "hola", [ 1 ] )

    select seed [ 0 ] tree == Just ( "hello", [ 0, 0 ] )

    select seed [ 0, 1 ] tree == Just ( "hell", [ 0, 1 ] )

    select seed [ 2 ] tree == Nothing

などのようになる。

-}
select : Random.Seed -> Path -> Tree -> Maybe ( String, Path )
select seed path tree =
    selectAux seed path tree 0


{-| Nodeを一つTreeから取得して、小説に付け加えようとする。
Pathが不正だと、ここでArray.getがNothingを返し、それが全体の戻り値になる。
-}
selectAux : Random.Seed -> Path -> Tree -> Int -> Maybe ( String, Path )
selectAux seed path tree currentIndex =
    Array.get currentIndex tree |> Maybe.andThen (appendNode seed path tree)


{-| nodeを小説に付け加えようとする。
-}
appendNode : Random.Seed -> Path -> Tree -> Node -> Maybe ( String, Path )
appendNode seed path tree node =
    let
        h =
            Maybe.withDefault "" node.node

        length =
            Array.length node.next
    in
    if length == 0 then
        Just ( h, [] )

    else if length == 1 then
        let
            {- Array.getは実際には必ず成功する。 -}
            nextIndex =
                Maybe.withDefault -1 (Array.get 0 node.next)
        in
        selectAux seed path tree nextIndex |> Maybe.map (\( resultNovel, resultPath ) -> ( h ++ resultNovel, resultPath ))

    else
        case path of
            [] ->
                let
                    indexGenerator =
                        Random.int 0 (length - 1)

                    ( choice, nextSeed ) =
                        Random.step indexGenerator seed

                    {- Array.getは実際には必ず成功する。 -}
                    nextIndex =
                        Maybe.withDefault -1 (Array.get choice node.next)
                in
                selectAux nextSeed path tree nextIndex |> Maybe.map (\( resultNovel, resultPath ) -> ( h ++ resultNovel, choice :: resultPath ))

            choice :: restPath ->
                let
                    {- pathが不正だとここで-1が返り、最終的にNothingが返る。 -}
                    nextIndex =
                        Maybe.withDefault -1 (Array.get choice node.next)
                in
                selectAux seed restPath tree nextIndex |> Maybe.map (\( resultNovel, resultPath ) -> ( h ++ resultNovel, choice :: resultPath ))
