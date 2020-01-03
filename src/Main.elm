module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, pre)
import Url
import Http
import Json.Decode exposing (..)
import Array exposing (Array , get, foldr, length)
import Random

-- MAIN

main : Program () Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }

-- MODEL
type alias Model =
  { key : Nav.Key
  , url : Url.Url
  , state : State
  , seed : Random.Seed
  }

type State
  = Failure String
  | Loading
  | Success NovelMaze
  
type alias NovelMaze = Array NovelNode

type alias NovelNode =
  { node : Maybe String
  , next : Array Int
  }


init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  ( Model key url Loading (Random.initialSeed 43)
  , Http.get
    { url = jsonUrl url
    , expect = Http.expectJson GotJson jsonDecoder
    }
  )

jsonUrl : Url.Url -> String
jsonUrl url = schemeAndAuthority url ++ "tree.json"

schemeAndAuthority : Url.Url -> String
schemeAndAuthority url =
  let 
    scheme =
      case url.protocol of
        Url.Https -> "https://"
        Url.Http -> "http://"
    host = url.host
    portString =
      case url.port_ of
        Nothing -> ""
        Just portNum -> ":" ++ (String.fromInt portNum)
  in
    scheme ++ host ++ portString ++ "/"  

-- UPDATE

jsonDecoder : Decoder NovelMaze
jsonDecoder =
  array (map2 NovelNode
          (field "node" (nullable string))
          (field "next" (array int)))

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotJson (Result Http.Error NovelMaze)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Nav.pushUrl model.key (Url.toString url))
        Browser.External href ->
          (model, Nav.load href)
    UrlChanged url ->
      ({ model | url = url }, Cmd.none)
    GotJson result ->
      case result of
        Ok novelMaze ->
          ({model | state = Success novelMaze}, Cmd.none)
        Err err ->
          ({model | state = Failure (errorToString err)}, Cmd.none)

errorToString err =
  case err of
    Http.BadUrl url -> "urlがおかしいです"
    Http.Timeout -> "タイムアウトが発生しました"
    Http.NetworkError -> "ネットワークがおかしいです"
    Http.BadStatus _ -> "ステータスがおかしいです"
    Http.BadBody message -> message

-- SUBSCRIPTION
subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW
view : Model -> Browser.Document Msg
view model =
  { title = "迷路小説"
  , body = 
    [ case model.state of
        Failure errorMessage ->
          text ("データのロードに失敗しました:" ++ errorMessage)
        Loading ->
          text "loading..."
        Success novelMaze ->
          randomNovel model.seed novelMaze |> text
    ]
  }

randomNovel : Random.Seed -> NovelMaze -> String
randomNovel seed novelMaze = randomNovelAux seed novelMaze 0

randomNovelAux : Random.Seed -> NovelMaze -> Int -> String
randomNovelAux seed novelMaze currentIndex =
  let 
    currentNode = get currentIndex novelMaze
  in
    case currentNode of
      Nothing -> ""
      Just node ->
        (case node.node of
          Nothing -> ""
          Just c -> c)
        ++
        (if length node.next == 0 
         then "" 
         else 
           let
             length = Array.length node.next
             indexGenerator = Random.int 0 (length - 1)
             (index, nextSeed) = Random.step indexGenerator seed
             nextIndex = (Maybe.withDefault 0 (get index node.next))
           in
             randomNovelAux nextSeed novelMaze nextIndex)

