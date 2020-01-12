module Main exposing (main)

import Array exposing (Array, foldr, get, length)
import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (class)
import Http
import Json.Decode as JD exposing (Decoder)
import Maze
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
    | Success NovelMaze


type alias NovelMaze =
    Array NovelNode


type alias NovelNode =
    { node : Maybe String
    , next : Array Int
    }



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


jsonDecoder : Decoder NovelMaze
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
    | GotJson (Result Http.Error NovelMaze)


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
                Ok novelMaze ->
                    ( { model | state = Success novelMaze }, Cmd.none )

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

            Success novelMaze ->
                randomMazeHtml model.seed novelMaze
        ]
    }


randomNovel : Random.Seed -> NovelMaze -> String
randomNovel seed novelMaze =
    randomNovelAux seed novelMaze 0


randomNovelAux : Random.Seed -> NovelMaze -> Int -> String
randomNovelAux seed novelMaze currentIndex =
    let
        currentNode =
            get currentIndex novelMaze
    in
    case currentNode of
        Nothing ->
            ""

        Just node ->
            (case node.node of
                Nothing ->
                    ""

                Just c ->
                    c
            )
                ++ (if length node.next == 0 then
                        ""

                    else
                        let
                            length =
                                Array.length node.next

                            indexGenerator =
                                Random.int 0 (length - 1)

                            ( index, nextSeed ) =
                                Random.step indexGenerator seed

                            nextIndex =
                                Maybe.withDefault 0 (get index node.next)
                        in
                        randomNovelAux nextSeed novelMaze nextIndex
                   )


randomMaze : Random.Seed -> NovelMaze -> Maybe Maze.Maze
randomMaze seed novelMaze =
    randomNovel seed novelMaze |> Maze.novelPath (Maze.randomChooser seed)


randomMazeHtml : Random.Seed -> NovelMaze -> Html msg
randomMazeHtml seed novelMaze =
    let
        maybeMaze =
            randomMaze seed novelMaze
    in
    case maybeMaze of
        Nothing ->
            div [ class "error" ] [ text "迷路作成に失敗しました。" ]

        Just maze ->
            let
                area =
                    Maze.getArea maze
            in
            div [] <| mazeRows area maze


mazeRows : Maze.Area -> Maze.Maze -> List (Html msg)
mazeRows area maze =
    mazeRowsAux area maze area.top []


mazeRowsAux : Maze.Area -> Maze.Maze -> Int -> List (Html msg) -> List (Html msg)
mazeRowsAux area maze row acc =
    if row < area.bottom then
        acc

    else
        mazeRowsAux area maze (row - 1) (acc ++ [ div [] (mazeColumns area maze row) ])


mazeColumns : Maze.Area -> Maze.Maze -> Int -> List (Html msg)
mazeColumns area maze row =
    mazeColumnsAux area maze row area.left []


mazeColumnsAux : Maze.Area -> Maze.Maze -> Int -> Int -> List (Html msg) -> List (Html msg)
mazeColumnsAux area maze row column acc =
    if column > area.right then
        acc

    else
        mazeColumnsAux area maze row (column + 1) (acc ++ [ mazeCell ( column, row ) maze ])


mazeCell : Maze.Cell -> Maze.Maze -> Html msg
mazeCell cell maze =
    let
        maybe =
            Maze.get cell maze
    in
    case maybe of
        Nothing ->
            span [class "wall"] [ text "\u{3000}" ]

        Just str ->
            span [class "path"] [ text str ]
