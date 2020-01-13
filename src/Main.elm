module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, article, div, span, text)
import Html.Attributes exposing (class, href)
import Http
import Json.Decode as JD exposing (Decoder)
import Maze exposing (Area, Cell, Maze)
import Novel exposing (NovelNode, NovelPath, NovelTree, randomNovel)
import Random
import Route exposing (Query, Route(..), urlToRoute)
import Task
import Time
import Url exposing (Url)
import Util exposing (jsonUrl, seedUrl)



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
    , url : Url
    , state : State
    , seed : Int
    , path : NovelPath
    }


type State
    = Failure String
    | Loading
    | Success NovelTree



-- INIT


dummySeed : Int
dummySeed =
    0


defaultPath : NovelPath
defaultPath =
    []


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case urlToRoute url of
        Nothing ->
            ( Model key url (Failure "お探しのページは見つかりません。") dummySeed defaultPath
            , Cmd.none
            )

        Just (Top query) ->
            case query.path of
                Nothing ->
                    seedInitialize url key defaultPath query.seed

                Just pathString ->
                    case Novel.pathFromString pathString of
                        Nothing ->
                            ( Model key url (Failure "お探しの迷路は道は見つかりません。") dummySeed defaultPath
                            , Cmd.none
                            )

                        Just path ->
                            seedInitialize url key path query.seed


seedInitialize : Url -> Nav.Key -> NovelPath -> Maybe Int -> ( Model, Cmd Msg )
seedInitialize url key path maybeSeed =
    case maybeSeed of
        Nothing ->
            ( Model key url Loading dummySeed path
            , Task.perform GotTime Time.now
            )

        Just seed ->
            ( Model key url Loading seed path
            , Http.get
                { url = jsonUrl url
                , expect = Http.expectJson GotJson jsonDecoder
                }
            )



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
    | UrlChanged Url
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
            ( { model | seed = Time.posixToMillis posix }
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

        Http.BadStatus status ->
            if status == 400 then
                "不正なリクエストです。"

            else if status == 404 then
                "お探しのページは存在しません。"

            else if status == 500 then
                "サーバー内部エラーが発生しました。"

            else
                "サーバーエラーが発生しました。"

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
                text errorMessage

            Loading ->
                text "loading..."

            Success novelTree ->
                randomMazeHtml model novelTree
        ]
    }


randomMaze : Model -> NovelTree -> ( Maze, String )
randomMaze model novelTree =
    let
        random =
            Random.initialSeed model.seed

        ( novel, novelPath ) =
            randomNovel random model.path novelTree

        chooser =
            Maze.randomChooser random

        maze =
            Maze.novelPath chooser novel

        pathString =
            Novel.pathToString novelPath
    in
    ( maze, pathString )


randomMazeHtml : Model -> NovelTree -> Html msg
randomMazeHtml model novelTree =
    let
        ( maze, pathString ) =
            randomMaze model novelTree

        area =
            Maze.getArea maze
    in
    article [ class "main" ]
        [ div [ class "maze" ] <| mazeRows area maze
        , div [ class "path" ] [ text pathString ]
        , div [ class "seed" ] [ text "ブックマーク用URL:", seedLink model ]
        ]


seedLink : Model -> Html msg
seedLink model =
    let
        link =
            seedUrl model.seed model.url
    in
    a [ href link ] [ text link ]


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
