module Maze exposing
    ( Coordinates, Kind(..), Cell, Maze
    , empty, insert
    , getChar, getKind
    , Area, getArea
    , makeExit
    , addBranch
    , Chooser(..), choose, next, randomChooser, choiceOfNextCoordinates
    , vonNeumannNeighborhood
    , canDig
    )

{-| 小説を二次元のセル上に配置した小説迷路に関するモジュール。


# Maze

@docs Coordinates, Kind, Cell, Maze


# Create

@docs empty, insert


# Get

@docs getChar, getKind


# Area

@docs Area, getArea


# Exit

@docs makeExit


# Branch

@docs addBranch


# Choose

@docs Chooser, choose, next, randomChooser, choiceOfNextCoordinates


# Neighborhood

@docs vonNeumannNeighborhood


# Dig

@docs canDig

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Path exposing (Path)
import Random
import Set exposing (Set)
import Util exposing (getNth)



-- MAZE


{-| `Maze`の`Cell`の座標を表す。
-}
type alias Coordinates =
    ( Int, Int )


{-| `Maze`の`Cell`の種類を表す。
-}
type Kind
    = Wall
    | Space
    | Start
    | Fork Path


{-| `Maze`の一つ一つの区画の内容を表す。。
-}
type alias Cell =
    { char : Char
    , kind : Kind
    }


{-| `Coordinates`に`Cell`を対応させて、道に文字列が並んだ小説迷路を表す。
例えば小説迷路

    　出
    　口は
    　　どこ

は、

    Maze.empty
        |> insert ( 1, 1 ) (Cell '出' Start)
        |> insert ( 1, 2 ) (Cell '口' Space)
        |> insert ( 2, 2 ) (Cell 'は' Space)
        |> insert ( 2, 3 ) (Cell 'ど' Space)
        |> insert ( 3, 3 ) (Cell 'こ' Space)

によって作れる。

-}
type alias Maze =
    Dict Coordinates Cell



-- CREATE


{-| 空の迷路。
-}
empty : Maze
empty =
    Dict.empty


{-| 迷路のセルに文字を挿入する。
-}
insert : Coordinates -> Cell -> Maze -> Maze
insert coordinates c maze =
    Dict.insert coordinates c maze



-- GET


{-| `Coordinates`に格納された文字を取得する。
デフォルト値は全角空白。
-}
getChar : Coordinates -> Maze -> Char
getChar coordinates maze =
    Dict.get coordinates maze
        |> Maybe.map .char
        |> Maybe.withDefault '\u{3000}'


{-| `Coordinates`に格納された`Cell`の種類を取得する。
デフォルト値は`Wall`。
-}
getKind : Coordinates -> Maze -> Kind
getKind coordinates maze =
    Dict.get coordinates maze
        |> Maybe.map .kind
        |> Maybe.withDefault Wall



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
                |> insert ( 2, 0 ) (Cell '迷' Start)
                |> insert ( 2, 1 ) (Cell '路' Space)
                |> insert ( 1, 1 ) (Cell 'の' Space)
                |> insert ( 0, 1 ) (Cell '中' Space)
                |> insert ( 0, 2 ) (Cell 'で' Space)
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



-- EXIT


{-| 迷路が完成した場合と、後戻りが必要な場合を分ける。
-}
type MazeResult
    = MazeResult Maze -- 完成した迷路
    | BackTrack Int -- 後戻りする数


{-| 文字列の最初の文字を返す。
文字列の長さが0の時は全角空白を返すが、必ず文字列の長さが1以上であることをチェックして使うこと。
-}
headChar : String -> Char
headChar =
    String.toList >> List.head >> Maybe.withDefault '\u{3000}'


{-| 文字列から一本道の迷路を作る。
これが迷路の出口への道になる
例えば、

    novelPath chooser "おはよう" [1]

とすることで

    お
    はよ
    　う

というような一本道の迷路が出来上がる。
ここで`'お'`の`Cell`が`Start`である。

またゴールである`'う'`の`Cell`は、
そこまでの道順の情報である`[1]`が格納された、
`Fork [1]`となる。
-}
makeExit : Chooser -> String -> Path -> Maze
makeExit chooser novel path =
    if String.isEmpty novel then
        empty

    else
        let
            revNovel =
                String.reverse novel

            c =
                headChar revNovel

            rest =
                String.dropLeft 1 revNovel
        in
        case makeExitAux chooser c rest empty Set.empty ( 0, 0 ) of
            {- 迷路が完成した場合。 -}
            MazeResult maze ->
                {- ゴールにはそこまでのPathの情報を格納する。 -}
                insert ( 0, 0 ) (Cell (getChar ( 0, 0 ) maze) (Fork path)) maze

            {- バックトラックで最初まで戻ってしまった時は、別の乱数を使う。 -}
            BackTrack _ ->
                let
                    nextChooser =
                        next chooser
                in
                makeExit nextChooser novel path


{-| 文字列から一つずつ文字を取って迷路に配置していく。
-}
makeExitAux : Chooser -> Char -> String -> Maze -> Set Coordinates -> Coordinates -> MazeResult
makeExitAux chooser currentChar currentRest maze exceptions currentCoordinates =
    if String.length currentRest == 0 then
        MazeResult (insert currentCoordinates (Cell currentChar Start) maze)

    else
        let
            maybeNextCoordinates =
                chooseNextCoordinates chooser maze exceptions currentCoordinates
        in
        case maybeNextCoordinates of
            {- 選べる道が存在しないとき、つまり行き止まりの時は、
               他の道を選んでも行き止まりの可能性が高いので定数だけ逆戻りする。
            -}
            Nothing ->
                BackTrack 10

            {- 試しに選んだ道を伸ばしてみる。 -}
            Just ( nextCoordinates, nextChooser ) ->
                let
                    c =
                        headChar currentRest

                    rest =
                        String.dropLeft 1 currentRest

                    nextMaze =
                        insert currentCoordinates (Cell currentChar Space) maze

                    {- 道をさらに伸ばす。 -}
                    result =
                        makeExitAux nextChooser c rest nextMaze Set.empty nextCoordinates
                in
                case result of
                    {- 逆戻りの途中の時はさらに逆戻りする。
                       逆戻りが完了した時は、前に選択した道を選択肢から除外してやり直す。
                    -}
                    BackTrack n ->
                        if n == 0 then
                            makeExitAux chooser currentChar currentRest maze (Set.insert nextCoordinates exceptions) currentCoordinates

                        else
                            BackTrack (n - 1)

                    {- 道が伸ばせて、迷路が完成したらなら、そのまま返す。 -}
                    _ ->
                        result



-- BRANCH


{-| 迷路のスタート地点を取得する。
-}
getStart : Maze -> Coordinates
getStart =
    Dict.toList
        >> List.filterMap
            (\( coordinates, { kind } ) ->
                if kind == Start then
                    Just coordinates

                else
                    Nothing
            )
        >> List.head
        >> Maybe.withDefault ( 0, 0 )


{-| 迷路の枝分かれから部分から行き止まりに到る分枝を作る。
-}
addBranch : Chooser -> String -> Path -> Path -> Maze -> Maze
addBranch chooser novel startPath endPath maze =
    let
        start =
            getStart maze
    in
    gotoFork chooser novel startPath endPath start start maze

{-| 既に出来ている迷路を辿って、分かれ道（辿っている小説と迷路の小説が食い違う地点）まで進む。
分かれ道へはそこまでの`Path`の情報を格納する。
-}
gotoFork : Chooser -> String -> Path -> Path -> Coordinates -> Coordinates -> Maze -> Maze
gotoFork  chooser currentRest startPath endPath previousCoordinates currentCoordinates maze =
    if String.length currentRest == 0 
    then maze
    else let
             nextChar = headChar currentRest
             nextRest = String.dropLeft 1 currentRest
         in
            case followExistingRoad maze nextChar previousCoordinates currentCoordinates of
                {- 分かれ道まで来た -}
                Nothing ->
                    let
                        forkChar = getChar currentCoordinates maze
                        {- 分かれ道には、そこまでのPathの情報を格納する。 -}
                        newMaze = insert currentCoordinates  (Cell forkChar (Fork startPath)) maze
                    in
                    makeBranch chooser nextChar nextRest endPath newMaze currentCoordinates

                {- まだ分かれていない -}
                Just nextCoordinates ->
                    gotoFork chooser nextRest startPath endPath currentCoordinates nextCoordinates maze


                

{-| 小説に沿って既に存在している道をいく時に、次の道を求める。
-}
followExistingRoad : Maze -> Char -> Coordinates -> Coordinates -> Maybe Coordinates 
followExistingRoad maze nextChar previousCoordinates  =
    vonNeumannNeighborhood
      >> Set.remove previousCoordinates
      >> Set.filter (\coordinatess -> getChar coordinatess maze == nextChar)
      >> Set.toList >> List.head

{-| 枝分かれ地点から枝を作っていく。
-}
makeBranch : Chooser -> Char -> String -> Path -> Maze -> Coordinates -> Maze
makeBranch chooser currentChar currentRest endPath maze currentCoordinates =
  case makeBranchAux chooser currentChar currentRest endPath maze Set.empty currentCoordinates of
  MazeResult resultMaze -> resultMaze

  BackTrack _ ->
      let
          nextChooser =
              next chooser
      in
      makeBranch nextChooser currentChar currentRest endPath maze currentCoordinates

{-| 枝分かれからスタートして、行き止まりまでを作る。
-}
makeBranchAux : Chooser -> Char -> String -> Path -> Maze -> Set Coordinates -> Coordinates -> MazeResult
makeBranchAux chooser currentChar currentRest endPath maze exceptions currentCoordinates =
    {- 分枝は最後までたどり着いては駄目。 -}
    if String.length currentRest == 0 then
        BackTrack 0

    else
        let
            maybeNextCoordinates =
                chooseNextCoordinates chooser maze exceptions currentCoordinates
        in
        case maybeNextCoordinates of
            {- 選べる道が存在しないとき、つまり行き止まりの時は、そこで終了。
            -}
            Nothing ->
                {- 行き止まりにはその最後までのPathの情報を格納する。 -}
                MazeResult (insert currentCoordinates (Cell currentChar (Fork endPath)) maze) 

            {- 試しに選んだ道を伸ばしてみる。 -}
            Just ( nextCoordinates, nextChooser ) ->
                let
                    c =
                        headChar currentRest

                    rest =
                        String.dropLeft 1 currentRest

                    nextMaze =
                        insert currentCoordinates (Cell currentChar Space) maze

                    {- 道をさらに伸ばす。 -}
                    result =
                        makeBranchAux nextChooser c rest endPath nextMaze Set.empty nextCoordinates
                in
                case result of
                    {- 逆戻りの回数はここでは無視して、すぐにトラックバックする。
                    -}
                    BackTrack _ ->
                        makeBranchAux chooser currentChar currentRest endPath maze (Set.insert nextCoordinates exceptions) currentCoordinates


                    {- 道が伸ばせて、迷路が完成したらなら、そのまま返す。 -}
                    _ ->
                        result
        
        
    


-- CHOOSE


{-| 空でない`Coordinates`の集合から一つCoordinatesを選択して新しい`Chooser`と一緒に返す。
`Coordinates`の集合が空の時は、`Nothing`を返す。
`Coordinates`の集合か空でない時は、`Nothing`を返してはいけない。
-}
type Chooser
    = Chooser (Set Coordinates -> Maybe ( Coordinates, Chooser ))


{-| `Chooser`を使って、`Coordinates`を選択する。
-}
choose : Chooser -> Set Coordinates -> Maybe ( Coordinates, Chooser )
choose (Chooser chooser) coordinatess =
    chooser coordinatess


{-| `Coordinates`を選択せずに、新しい`Chooser`だけを手に入れる。
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


{-| 既に作られた迷路と候補の除外リストと現在のセルから次のセルをChooserを使って選べたなら選ぶ。
-}
chooseNextCoordinates : Chooser -> Maze -> Set Coordinates -> Coordinates -> Maybe ( Coordinates, Chooser )
chooseNextCoordinates chooser maze exceptions currentCoordinates =
    choiceOfNextCoordinates maze exceptions currentCoordinates |> choose chooser


{-| 既に作られた迷路と候補の除外リストと現在のCoordinatesから、次のセルの候補を返す。
-}
choiceOfNextCoordinates : Maze -> Set Coordinates -> Coordinates -> Set Coordinates
choiceOfNextCoordinates maze exceptions currentCoordinates =
    vonNeumannNeighborhood currentCoordinates
        |> (\set ->
                Set.diff set exceptions
                    |> Set.filter (\coordinates -> canDig maze currentCoordinates coordinates)
           )



-- NEIGHBORHOOD


{-| セルのフォン・ノイマン近傍

     ■
    ■□■
     ■

Coordinates`□`の周りの上下左右の`■`四つのこと。

-}
vonNeumannNeighborhood : Coordinates -> Set Coordinates
vonNeumannNeighborhood coordinates =
    let
        ( x, y ) =
            coordinates
    in
    Set.fromList [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]



-- DIG


{-| 迷路のセルが掘って道にできるかどうかを返す。
迷路のセルが掘れるのは、エリア内の新しい道であり、新しい交差点を作らないとき、すなわち次の場合である。

  - その道がエリア内、つまりy座標が0以上
  - 既に掘られて道になっていない
  - 既に掘られて道になっているセルで一つ前のセルでないセルと隣接していない

-}
canDig : Maze -> Coordinates -> Coordinates -> Bool
canDig maze previousCoordinates coordinates =
    inArea coordinates && (not <| onExistingPath maze coordinates) && doesBecomeSinglePath maze previousCoordinates coordinates


{-| エリア内にあるか。
-}
inArea : Coordinates -> Bool
inArea ( x, y ) =
    y >= 0


{-| 既に作られた道になっているか。
例えば
"既に道だ"
という文字列を

    既に道

と道を作って、

    insert ( 1, 0 ) (Cell 'だ' Space)

としようとすると、既に道になっている`Coordinates`へ後戻りしようとしているので、
これは正しい道の堀り方ではない。

-}
onExistingPath : Maze -> Coordinates -> Bool
onExistingPath maze coordinates =
    Dict.member coordinates maze


{-| 一本道になるか。
例えば
"一本道にならない"
という文字列を

    一いな
    本　ら
    道にな

と道を作ると一本道にならないので、正しい道の掘り方ではない。

-}
doesBecomeSinglePath : Maze -> Coordinates -> Coordinates -> Bool
doesBecomeSinglePath maze previousCoordinates coordinates =
    let
        neighborhood =
            vonNeumannNeighborhood coordinates

        neighborhoodWithoutPrevious =
            Set.remove previousCoordinates neighborhood
    in
    Set.filter (onExistingPath maze) neighborhoodWithoutPrevious |> Set.isEmpty
