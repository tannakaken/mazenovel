module Novel exposing
    ( Tree
    , Node
    , select
    )

{-| 木構造になった小説のデータ構造に関するモジュール。


# Novel

@docs Tree
@docs Node


# Select

@docs select

-}

import Array exposing (Array)
import Path exposing (Path)
import Random
import Set exposing (Set)



-- NOVEL


{-| 途中から分岐する小説を表す、木構造のデータ構造
-}
type alias Tree =
    Array Node


{-| nodeは小説の一文字を、
nextは次の文字を格納したNodeのTree内でのindexを表す。
再帰的データ構造にすると、jsonをパースして読み込む際にstackが溢れたのでこのようなデータ構造にした。
-}
type alias Node =
    { node : Maybe String
    , next : Array Int
    }



-- SELECT


{-| Pathに従ってNovelTreeを辿り、Pathが途切れたあとは、ランダムに選ぶ。
最終的に生成された小説と、辿ってきたNovelPathを返す。
Pathが不正で、そのような道が存在しない時はNothingを返す。
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
