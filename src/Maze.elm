module Maze exposing (..)

import Array exposing (Array)
import Dict exposing (Dict)
import Random
import Set exposing (Set)


type alias Cell =
    ( Int, Int )


type alias Maze =
    Dict Cell String


empty : Maze
empty =
    Dict.empty


insert : Cell -> String -> Maze -> Maze
insert cell c maze =
    Dict.insert cell c maze


get : Cell -> Maze -> Maybe String
get cell maze =
    Dict.get cell maze


{-| 空でないCellの集合から一つCellを選択して新しいChooserと一緒に返す。
Cellの集合が空の時は、Nothingを返す。
Cellの集合か空でない時は、Nothingを返してはいけない。
-}
type Chooser
    = Chooser (Set Cell -> Maybe ( Cell, Chooser ))


choose : Chooser -> Set Cell -> Maybe ( Cell, Chooser )
choose (Chooser chooser) cells =
    chooser cells


{-| Cellを選択せずに、新しいChooserだけを手に入れる。
-}
next : Chooser -> Chooser
next chooser =
    case choose chooser (Set.fromList [ ( 0, 0 ) ]) of
        Just ( _, nextChooser ) ->
            nextChooser

        {- Chooserが正しく設計されていれば、この節は実行されない。 -}
        Nothing ->
            chooser


getNth : Int -> List a -> Maybe a
getNth nth list =
    list |> List.drop nth |> List.head


randomChooser : Random.Seed -> Chooser
randomChooser seed =
    let
        chooser =
            \set ->
                let
                    list =
                        Set.toList set

                    length =
                        Set.size set
                in
                if length == 0 then
                    Nothing

                else
                    let
                        indexGenerator =
                            Random.int 0 (length - 1)

                        ( index, nextSeed ) =
                            Random.step indexGenerator seed

                        maybe =
                            getNth index list
                    in
                    case maybe of
                        Nothing ->
                            Nothing

                        Just x ->
                            Just ( x, randomChooser nextSeed )
    in
    Chooser chooser


type alias Area =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


getArea : Maze -> Area
getArea maze =
    Dict.keys maze
        |> List.foldl (\( x, y ) { top, right, bottom, left } -> Area (max y top) (max x right) (min y bottom) (min x left))
            (Area 0 0 0 0)


{-| 迷路が完成した場合と、後戻りが必要な場合を分ける。
-}
type MazeResult {- 完成した迷路 -}
    = MazeResult Maze {- 後戻りする数 -}
    | BackTrack Int


{-| 文字列から迷路の一本道を作る。
-}
novelPath : Chooser -> String -> Maybe Maze
novelPath chooser novel =
    if String.isEmpty novel then
        Nothing

    else
        let
            c =
                String.left 1 novel

            rest =
                String.dropLeft 1 novel
        in
        case novelPathAux chooser c rest Dict.empty Set.empty ( 0, 0 ) of
            {- 迷路が完成した場合。 -}
            MazeResult maze ->
                Just maze

            {- バックトラックで最初まで戻ってしまった時は、別の乱数を使う。 -}
            BackTrack _ ->
                let
                    nextChooser =
                        next chooser
                in
                novelPath nextChooser novel


{-| 文字列から一つずつ文字を取って迷路に配置していく。
-}
novelPathAux : Chooser -> String -> String -> Maze -> Set Cell -> Cell -> MazeResult
novelPathAux chooser currentChar currentRest maze exceptions currentCell =
    if String.length currentRest == 0 then
        MazeResult (insert currentCell currentChar maze)

    else
        let
            maybeNextCell =
                chooseNextCell chooser maze exceptions currentCell
        in
        case maybeNextCell of
            {- 選べる道が存在しないとき、つまり行き止まりの時は、
               他の道を選んでも行き止まりの可能性が高いので定数だけ逆戻りする。
            -}
            Nothing ->
                BackTrack 10

            {- 試しに選んだ道を伸ばしてみる。 -}
            Just ( nextCell, nextChooser ) ->
                let
                    c =
                        String.left 1 currentRest

                    rest =
                        String.dropLeft 1 currentRest

                    nextMaze =
                        insert currentCell currentChar maze

                    {- 道をさらに伸ばす。 -}
                    result =
                        novelPathAux nextChooser c rest nextMaze Set.empty nextCell
                in
                case result of
                    {- 逆戻りの途中の時はさらに逆戻りする。
                       逆戻りが完了した時は、前に選択した道を選択肢から除外してやり直す。
                    -}
                    BackTrack n ->
                        if n == 0 then
                            novelPathAux chooser currentChar currentRest maze (Set.insert nextCell exceptions) currentCell

                        else
                            BackTrack (n - 1)

                    {- 道が伸ばせて、迷路が完成したらなら、そのまま返す。 -}
                    _ ->
                        result


{-| 既に作られた迷路と現在のセルから次のセルをChooserを使って選べたなら選ぶ。
-}
chooseNextCell : Chooser -> Maze -> Set Cell -> Cell -> Maybe ( Cell, Chooser )
chooseNextCell chooser maze exceptions currentCell =
    choiceOfNextCell maze exceptions currentCell |> choose chooser


choiceOfNextCell : Maze -> Set Cell -> Cell -> Set Cell
choiceOfNextCell maze exceptions currentCell =
    vonNeumannNeighborhood currentCell
        |> (\set ->
                Set.diff set exceptions
                    |> Set.filter (\cell -> canDig maze currentCell cell)
           )


vonNeumannNeighborhood : Cell -> Set Cell
vonNeumannNeighborhood cell =
    let
        ( x, y ) =
            cell
    in
    Set.fromList [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]


{-| 迷路のセルが掘って道にできるかどうかを返す。
迷路のセルが掘れるのは、エリア内の新しい道であり、新しい交差点を作らないとき、すなわち次の場合である。

  - その道がエリア内、つまりy座標が0以上
  - 既に掘られて道になっていない
  - 既に掘られて道になっているセルで一つ前のセルでないセルと隣接していない

-}
canDig : Maze -> Cell -> Cell -> Bool
canDig maze previousCell cell =
    inArea cell && (not <| onExistingPath maze cell) && doesBecomeSinglePath maze previousCell cell


inArea : Cell -> Bool
inArea ( x, y ) =
    y >= 0


onExistingPath : Maze -> Cell -> Bool
onExistingPath maze cell =
    Dict.member cell maze


doesBecomeSinglePath : Maze -> Cell -> Cell -> Bool
doesBecomeSinglePath maze previousCell cell =
    let
        neighborhood =
            vonNeumannNeighborhood cell

        neighborhoodWithoutPrevious =
            Set.remove previousCell neighborhood
    in
    Set.filter (onExistingPath maze) neighborhoodWithoutPrevious |> Set.isEmpty
