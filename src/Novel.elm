module Novel exposing (NovelNode, NovelTree, pathFromString, pathToString, randomNovel)

import Array exposing (Array)
import Random


type alias NovelTree =
    Array NovelNode


type alias NovelNode =
    { node : Maybe String
    , next : Array Int
    }


type alias NovelPath =
    List Int


pathToString : NovelPath -> String
pathToString path =
    List.map String.fromInt path |> String.join ","


pathFromString : String -> NovelPath
pathFromString str =
    String.split "," str |> List.map (String.toInt >> Maybe.withDefault 0)


randomNovel : Random.Seed -> NovelTree -> ( String, NovelPath )
randomNovel seed novelTree =
    randomNovelAux seed novelTree 0


randomNovelAux : Random.Seed -> NovelTree -> Int -> ( String, NovelPath )
randomNovelAux seed novelTree currentIndex =
    let
        currentNode =
            Array.get currentIndex novelTree
    in
    case currentNode of
        {- NovelTreeが正しく設計されていればこの節は実行されないはず -}
        Nothing ->
            ( "", [] )

        Just node ->
            let
                h =
                    Maybe.withDefault "" node.node

                length =
                    Array.length node.next
            in
            if length == 0 then
                ( h, [] )

            else if length == 1 then
                let
                    {- Array.getは実際には必ず成功するはず -}
                    nextIndex =
                        Maybe.withDefault -1 (Array.get 0 node.next)

                    ( restNovel, path ) =
                        randomNovelAux seed novelTree nextIndex
                in
                ( h ++ restNovel, path )

            else
                let
                    indexGenerator =
                        Random.int 0 (length - 1)

                    ( choice, nextSeed ) =
                        Random.step indexGenerator seed

                    {- Array.getは実際には必ず成功するはず -}
                    nextIndex =
                        Maybe.withDefault -1 (Array.get choice node.next)

                    ( restNovel, restPath ) =
                        randomNovelAux nextSeed novelTree nextIndex
                in
                ( h ++ restNovel, choice :: restPath )
