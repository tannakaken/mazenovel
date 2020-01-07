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


{-| 文字列から迷路の一本道を作る
|
-}
randomPath : String -> Maze
randomPath novel =
    randomPathAux novel Dict.empty ( 0, 0 )


{-| 文字列から一つずつ文字を取って迷路に配置していく。
|
-}
randomPathAux : String -> Maze -> Cell -> Maze
randomPathAux novel maze currentCell =
    Dict.empty


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
    inArea cell && (not <| onExistingPath maze cell) && makeSinglePath maze cell previousCell


inArea : Cell -> Bool
inArea ( x, y ) =
    y >= 0


onExistingPath : Maze -> Cell -> Bool
onExistingPath maze cell =
    Dict.member cell maze


makeSinglePath : Maze -> Cell -> Cell -> Bool
makeSinglePath maze previousCell cell =
    let
        neighborhood =
            vonNeumannNeighborhood cell

        neighborhoodWithoutPrevious =
            Set.remove previousCell neighborhood
    in
    Set.filter (onExistingPath maze) neighborhoodWithoutPrevious |> Set.isEmpty
