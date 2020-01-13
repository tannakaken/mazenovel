module Novel exposing (NovelNode, NovelPath, NovelTree, pathFromString, pathToString, randomNovel)

import Array exposing (Array)
import Random


{-| 途中から分岐する小説を表す、木構造のデータ構造
-}
type alias NovelTree =
    Array NovelNode


{-| nodeは小説の一文字を、
nextは次の文字を格納したNovelNodeのNovelTree内でのindexを表す。
再帰的データ構造にすると、jsonをパースして読み込む際にstackが溢れたのでこのようなデータ構造にした。
-}
type alias NovelNode =
    { node : Maybe String
    , next : Array Int
    }


{-| 迷路の道順を表すデータ構造。
分かれ道で何番目の道を選ぶかのList
-}
type alias NovelPath =
    List Int


{-| NovelPath[0,1,2]を"0,1,2"のように、コンマ区切りの文字列表現にする。
-}
pathToString : NovelPath -> String
pathToString path =
    List.map String.fromInt path |> String.join ","


combineMaybe : List (Maybe a) -> Maybe (List a)
combineMaybe =
    List.foldr (Maybe.map2 (::)) (Just [])


{-| "0,1,2"のようなコンマ区切りの文字列を、[0,1,2]とNovelPathに変換する。
"0,a,2"のように整数でない文字列が混ざったらNothingを返す。
-}
pathFromString : String -> Maybe NovelPath
pathFromString str =
    String.split "," str
        |> List.map String.toInt
        |> combineMaybe


{-| NovelPathに従ってNovelTreeを辿り、NovelPathが途切れたあとは、ランダムに選ぶ。
最終的に生成された小説と、辿ってきたNovelPathを返す。
NovelPathが不正で、そのような道が存在しない時はNothingを返す。
-}
randomNovel : Random.Seed -> NovelPath -> NovelTree -> Maybe ( String, NovelPath )
randomNovel seed novelPath novelTree =
    randomNovelAux seed novelPath novelTree 0


{-| NovelNodeを一つNovelTreeから取得して、小説に付け加えようとする。
NovelPathが不正だと、ここでArray.getがNothingを返し、それが全体の戻り値になる。
-}
randomNovelAux : Random.Seed -> NovelPath -> NovelTree -> Int -> Maybe ( String, NovelPath )
randomNovelAux seed novelPath novelTree currentIndex =
    Array.get currentIndex novelTree |> Maybe.andThen (appendNode seed novelPath novelTree)


{-| NovelNodeを小説に付け加えようとする。
-}
appendNode : Random.Seed -> NovelPath -> NovelTree -> NovelNode -> Maybe ( String, NovelPath )
appendNode seed novelPath novelTree node =
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
        randomNovelAux seed novelPath novelTree nextIndex |> Maybe.map (\( restNovel, path ) -> ( h ++ restNovel, path ))

    else
        case novelPath of
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
                randomNovelAux nextSeed novelPath novelTree nextIndex |> Maybe.map (\( restNovel, restPath ) -> ( h ++ restNovel, choice :: restPath ))

            choice :: rest ->
                let
                    {- novelPathが不正だとここで-1が返り、最終的にNothingが返る。 -}
                    nextIndex =
                        Maybe.withDefault -1 (Array.get choice node.next)
                in
                randomNovelAux seed rest novelTree nextIndex |> Maybe.map (\( restNovel, restPath ) -> ( h ++ restNovel, choice :: restPath ))
