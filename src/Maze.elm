module Maze exposing
    ( Cell
    , Maze
    , empty
    , insert
    , get
    , Chooser(..)
    , choose
    , next
    , randomChooser
    , Area
    , getArea
    , makeExit
    , choiceOfNextCell
    , vonNeumannNeighborhood
    , canDig
    )

{-| 小説を二次元のセル上に配置した小説迷路に関するモジュール。


# Maze

@docs Cell
@docs Maze


# Create

@docs empty
@docs insert


# Get

@docs get


# Choose

@docs Chooser
@docs choose
@docs next
@docs randomChooser


# Area

@docs Area
@docs getArea


# Exit

@docs makeExit
@docs choiceOfNextCell


# Neighborhood

@docs vonNeumannNeighborhood
@docs canDig

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Random
import Set exposing (Set)
import Util exposing (getNth)



-- MAZE


{-| 迷路のセルの座標を表す。
-}
type alias Cell =
    ( Int, Int )


{-| Cellに文字を対応させて、道に文字列が並んだ小説迷路を表す。
例えば小説迷路

    　出
    　口は
    　　どこ

は、

    Maze.empty
        |> Maze.insert ( 1, 1 ) '出'
        |> Maze.insert ( 1, 2 ) '口'
        |> Maze.insert ( 2, 2 ) 'は'
        |> Maze.insert ( 2, 3 ) 'ど'
        |> Maze.insert ( 3, 3 ) 'こ'

によって作れる。

-}
type alias Maze =
    Dict Cell Char



-- CREATE


{-| 空の迷路。
-}
empty : Maze
empty =
    Dict.empty


{-| 迷路のセルに文字を挿入する。
-}
insert : Cell -> Char -> Maze -> Maze
insert cell c maze =
    Dict.insert cell c maze



-- GET


{-| Cellに格納された文字を取得する。
-}
get : Cell -> Maze -> Maybe Char
get cell maze =
    Dict.get cell maze



-- CHOOSE


{-| 空でない`Cell`の集合から一つCellを選択して新しい`Chooser`と一緒に返す。
`Cell`の集合が空の時は、`Nothing`を返す。
`Cell`の集合か空でない時は、`Nothing`を返してはいけない。
-}
type Chooser
    = Chooser (Set Cell -> Maybe ( Cell, Chooser ))


{-| `Chooser`を使って、`Cell`を選択する。
-}
choose : Chooser -> Set Cell -> Maybe ( Cell, Chooser )
choose (Chooser chooser) cells =
    chooser cells


{-| `Cell`を選択せずに、新しい`Chooser`だけを手に入れる。
-}
next : Chooser -> Chooser
next chooser =
    case choose chooser (Set.fromList [ ( 0, 0 ) ]) of
        Just ( _, nextChooser ) ->
            nextChooser

        {- Chooserが正しく設計されていれば、この節は実行されない。 -}
        Nothing ->
            chooser


{-| 擬似乱数を使用した`Chooser`。
-}
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



-- AREA


{-| 迷路の範囲を表す型。
-}
type alias Area =
    { top : Int
    , right : Int
    , bottom : Int
    , left : Int
    }


{-| その迷路から、その迷路が含まれる範囲を返す。

    迷路
    　の
    　中で

のAreaを求めると

    let
        maze =
            Maze.empty
                |> Maze.insert ( 2, 0 ) '迷'
                |> Maze.insert ( 2, 1 ) '路'
                |> Maze.insert ( 1, 1 ) 'の'
                |> Maze.insert ( 0, 1 ) '中'
                |> Maze.insert ( 0, 2 ) 'で'
    in
    Maze.getArea maze
        == { top = 2
           , right = 2
           , bottom = 0
           , left = 0
           }

となる。

-}
getArea : Maze -> Area
getArea maze =
    Dict.keys maze
        |> List.foldl (\( x, y ) { top, right, bottom, left } -> Area (max y top) (max x right) (min y bottom) (min x left))
            (Area 0 0 0 0)



-- EXITH


{-| 迷路が完成した場合と、後戻りが必要な場合を分ける。
-}
type MazeResult {- 完成した迷路 -}
    = MazeResult Maze {- 後戻りする数 -}
    | BackTrack Int


{-| 文字列の最初の文字を返す。
文字列の長さが0の時は全角空白を返すが、必ず文字列の長さが1以上であることをチェックして使うこと。
-}
headChar : String -> Char
headChar =
    String.toList >> List.head >> Maybe.withDefault '\u{3000}'


{-| 文字列から一本道の迷路を作る。
これが迷路の出口への道になる
例えば、

    novelPath chooser "おはよう"

とすることで

    お
    はよ
    　う

というような一本道の迷路が出来上がる。

-}
makeExit : Chooser -> String -> Maze
makeExit chooser novel =
    if String.isEmpty novel then
        empty

    else
        let
            c =
                headChar novel

            rest =
                String.dropLeft 1 novel
        in
        case makeExitAux chooser c rest empty Set.empty ( 0, 0 ) of
            {- 迷路が完成した場合。 -}
            MazeResult maze ->
                maze

            {- バックトラックで最初まで戻ってしまった時は、別の乱数を使う。 -}
            BackTrack _ ->
                let
                    nextChooser =
                        next chooser
                in
                makeExit nextChooser novel


{-| 文字列から一つずつ文字を取って迷路に配置していく。
-}
makeExitAux : Chooser -> Char -> String -> Maze -> Set Cell -> Cell -> MazeResult
makeExitAux chooser currentChar currentRest maze exceptions currentCell =
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
                        headChar currentRest

                    rest =
                        String.dropLeft 1 currentRest

                    nextMaze =
                        insert currentCell currentChar maze

                    {- 道をさらに伸ばす。 -}
                    result =
                        makeExitAux nextChooser c rest nextMaze Set.empty nextCell
                in
                case result of
                    {- 逆戻りの途中の時はさらに逆戻りする。
                       逆戻りが完了した時は、前に選択した道を選択肢から除外してやり直す。
                    -}
                    BackTrack n ->
                        if n == 0 then
                            makeExitAux chooser currentChar currentRest maze (Set.insert nextCell exceptions) currentCell

                        else
                            BackTrack (n - 1)

                    {- 道が伸ばせて、迷路が完成したらなら、そのまま返す。 -}
                    _ ->
                        result


{-| 既に作られた迷路と候補の除外リストと現在のセルから次のセルをChooserを使って選べたなら選ぶ。
-}
chooseNextCell : Chooser -> Maze -> Set Cell -> Cell -> Maybe ( Cell, Chooser )
chooseNextCell chooser maze exceptions currentCell =
    choiceOfNextCell maze exceptions currentCell |> choose chooser


{-| 既に作られた迷路と候補の除外リストと現在のCellから、次のセルの候補を返す。
-}
choiceOfNextCell : Maze -> Set Cell -> Cell -> Set Cell
choiceOfNextCell maze exceptions currentCell =
    vonNeumannNeighborhood currentCell
        |> (\set ->
                Set.diff set exceptions
                    |> Set.filter (\cell -> canDig maze currentCell cell)
           )



-- NEIGHBORHOOD


{-| セルのフォン・ノイマン近傍

     ■
    ■□■
     ■

Cell`□`の周りの上下左右の`■`四つのこと。

-}
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


{-| エリア内にあるか。
-}
inArea : Cell -> Bool
inArea ( x, y ) =
    y >= 0


{-| 既に作られた道になっているか。
例えば
"既に道だ"
という文字列を

    既に道

と道を作って、
Maze.insert (1,0) 'だ'
としようとすると、既に道になっているCellへ後戻りしようとしているので、
これは正しい道の堀り方ではない。

-}
onExistingPath : Maze -> Cell -> Bool
onExistingPath maze cell =
    Dict.member cell maze


{-| 一本道になるか。
例えば
"一本道にならない"
という文字列を

    一いな
    本　ら
    道にな

と道を作ると一本道にならないので、正しい道の掘り方ではない。

-}
doesBecomeSinglePath : Maze -> Cell -> Cell -> Bool
doesBecomeSinglePath maze previousCell cell =
    let
        neighborhood =
            vonNeumannNeighborhood cell

        neighborhoodWithoutPrevious =
            Set.remove previousCell neighborhood
    in
    Set.filter (onExistingPath maze) neighborhoodWithoutPrevious |> Set.isEmpty
