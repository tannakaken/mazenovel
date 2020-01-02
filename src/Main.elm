module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, pre)
import Url
import Http
import Json.Decode exposing (..)
import Array exposing (Array , get)

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
  }

type State
  = Failure String
  | Loading
  | Success String

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
  ( Model key url Loading
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

type alias NovelMaze = Array NovelNode

type alias NovelNode =
  { node : Maybe String
  , next : Array Int
  }

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
          case get 1 novelMaze of
            Nothing -> ({model | state = Failure "empty json"}, Cmd.none)
            Just head -> ({model | state = Success (Maybe.withDefault "null" head.node)}, Cmd.none)
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
        Success fullText ->
          pre [] [ text fullText ]
    ]
  }


