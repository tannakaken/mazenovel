module Util exposing (..)


getNth : Int -> List a -> Maybe a
getNth nth list =
    list |> List.drop nth |> List.head
