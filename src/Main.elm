module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode as JD exposing (Decoder)
import Maze exposing (Area, Cell, Maze)
import Novel exposing (NovelNode, NovelTree, randomNovel)
import Random
import Task
import Time
import Url
import Url.Parser as UP exposing (Parser, query, s)
import Url.Parser.Query as Q



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
    | Success NovelTree



-- INIT


type Route
    = Top (Maybe Int)


routeParser : Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Top (query <| Q.int "seed") ]


urlToRoute : Url.Url -> Maybe Route
urlToRoute url =
    UP.parse routeParser url


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case urlToRoute url of
        Nothing ->
            ( Model key url Loading (Random.initialSeed 0)
            , Task.perform GotTime Time.now
            )

        Just (Top maybeSeed) ->
            case maybeSeed of
                Nothing ->
                    ( Model key url Loading (Random.initialSeed 0)
                    , Task.perform GotTime Time.now
                    )

                Just seed ->
                    ( Model key url Loading (Random.initialSeed seed)
                    , Http.get
                        { url = jsonUrl url
                        , expect = Http.expectJson GotJson jsonDecoder
                        }
                    )



-- JSON


jsonUrl : Url.Url -> String
jsonUrl url =
    baseUrl url ++ "tree.json"


baseUrl : Url.Url -> String
baseUrl url =
    let
        scheme =
            case url.protocol of
                Url.Https ->
                    "https://"

                Url.Http ->
                    "http://"

        host =
            url.host

        portString =
            case url.port_ of
                Nothing ->
                    ""

                Just portNum ->
                    ":" ++ String.fromInt portNum
    in
    scheme ++ host ++ portString ++ String.replace "index.html" "" url.path



-- UPDATE


jsonDecoder : Decoder NovelTree
jsonDecoder =
    JD.array
        (JD.map2 NovelNode
            (JD.field "c" (JD.nullable JD.string))
            (JD.field "n" (JD.array JD.int))
        )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotTime Time.Posix
    | GotJson (Result Http.Error NovelTree)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        GotTime posix ->
            ( { model | seed = Random.initialSeed (Time.posixToMillis posix) }
            , Http.get
                { url = jsonUrl model.url
                , expect = Http.expectJson GotJson jsonDecoder
                }
            )

        GotJson result ->
            case result of
                Ok novelTree ->
                    ( { model | state = Success novelTree }, Cmd.none )

                Err err ->
                    ( { model | state = Failure (errorToString err) }, Cmd.none )


errorToString err =
    case err of
        Http.BadUrl url ->
            "urlがおかしいです"

        Http.Timeout ->
            "タイムアウトが発生しました"

        Http.NetworkError ->
            "ネットワークがおかしいです"

        Http.BadStatus _ ->
            "ステータスがおかしいです"

        Http.BadBody message ->
            message



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

            Success novelTree ->
                randomMazeHtml model.seed novelTree
        ]
    }


randomMaze : Random.Seed -> NovelTree -> Maze
randomMaze seed novelTree =
    let 
        (novel, _) = randomNovel seed novelTree
    in
        Maze.novelPath (Maze.randomChooser seed) novel


randomMazeHtml : Random.Seed -> NovelTree -> Html msg
randomMazeHtml seed novelTree =
    let
        maze =
            randomMaze seed novelTree
        area =
            Maze.getArea maze
    in
        div [] <| mazeRows area maze


mazeRows : Area -> Maze -> List (Html msg)
mazeRows area maze =
    mazeRowsAux area maze area.top []


mazeRowsAux : Area -> Maze -> Int -> List (Html msg) -> List (Html msg)
mazeRowsAux area maze row acc =
    if row < area.bottom then
        acc

    else
        mazeRowsAux area maze (row - 1) (acc ++ [ div [] (mazeColumns area maze row) ])


mazeColumns : Area -> Maze -> Int -> List (Html msg)
mazeColumns area maze row =
    mazeColumnsAux area maze row area.left []


mazeColumnsAux : Area -> Maze -> Int -> Int -> List (Html msg) -> List (Html msg)
mazeColumnsAux area maze row column acc =
    if column > area.right then
        acc

    else
        mazeColumnsAux area maze row (column + 1) (acc ++ [ mazeCell ( column, row ) maze ])


mazeCell : Cell -> Maze -> Html msg
mazeCell cell maze =
    let
        maybe =
            Maze.get cell maze
    in
    case maybe of
        Nothing ->
            span [ class "wall" ] [ text "\u{3000}" ]

        Just str ->
            span [ class "path" ] [ text str ]
