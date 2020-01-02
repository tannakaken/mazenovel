module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, pre)
import Url
import Http
import Json.Decode exposing (..)

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
  = Failure
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

type alias NovelMaze =
  { node : Maybe String
  , next : Maybe Choices
  }

type Choices = Choices (List NovelMaze)

jsonDecoder : Decoder NovelMaze
jsonDecoder =
  map2 NovelMaze
       (field "node" (nullable string))
       (maybe (field "next" (lazy (\_ -> choicesDecoder))))

choicesDecoder : Decoder Choices
choicesDecoder =
  map Choices (list jsonDecoder)

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
          ({model | state = Success (Maybe.withDefault "null" novelMaze.node)}, Cmd.none)
        Err _ ->
          ({model | state = Failure}, Cmd.none)

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
        Failure ->
          text "データのロードに失敗しました。"
        Loading ->
          text "loading..."
        Success fullText ->
          pre [] [ text fullText ]
    ]
  }


