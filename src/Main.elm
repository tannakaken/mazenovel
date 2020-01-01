module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, text, pre)
import Url
import Http

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
    , expect = Http.expectString GotText
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

type Msg
  = LinkClicked Browser.UrlRequest
  | UrlChanged Url.Url
  | GotText (Result Http.Error String)

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
    GotText result ->
      case result of
        Ok fullText ->
          ({model | state = Success fullText}, Cmd.none)
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
          text "I was unable to load your book."
        Loading ->
          text "loading..."
        Success fullText ->
          pre [] [ text fullText ]
    ]
  }


