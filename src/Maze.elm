module Maze exposing (..)

import Random
import Dict exposing (Dict)
import Array exposing (Array)

type alias Cell = (Int, Int)
type alias Maze = Dict Cell String

{-| 
  文字列から迷路の一本道を作る  
|-}
randomPath : String -> Maze
randomPath novel =
  randomPathAux novel Dict.empty (0,0)

{-|
  文字列から一つずつ文字を取って迷路に配置していく。
|-}
randomPathAux : String -> Maze -> Cell ->Maze
randomPathAux novel maze currentCell =
  Dict.empty

vonNeumannNeighborhood : Cell -> Array Cell
vonNeumannNeighborhood cell = Array.empty

canDig : Cell -> Cell -> Maze -> Bool
canDig cell previousCell maze = False
  
