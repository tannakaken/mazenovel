module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, address, article, dd, div, dl, dt, footer, h1, header, li, p, span, text, time, ul)
import Html.Attributes exposing (class, href, id)
import Http
import Json.Decode as JD exposing (Decoder)
import Maze exposing (Area, Coordinates, Maze)
import Novel
import Path exposing (Path)
import Random
import Route exposing (Query, Route(..), urlToRoute)
import Set
import Task
import Time
import Url exposing (Url)
import Util exposing (getNth, jsonUrl, urlForBookmark)



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
    , path : Path
    }


type State
    = Failure String
    | Loading
    | Success Novel.Tree



-- INIT


dummySeed : Int
dummySeed =
    0


defaultPath : Path
defaultPath =
    []


pathNotFound : String
pathNotFound =
    "お探しの道は見つかりませんでした。"


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case urlToRoute url of
        Nothing ->
            ( Model key url (Failure "お探しのページは見つかりませんでした。") dummySeed defaultPath
            , Cmd.none
            )

        Just (Top query) ->
            case query.path of
                Nothing ->
                    seedInitialize url key defaultPath query.seed

                Just pathString ->
                    case Path.fromString pathString of
                        Nothing ->
                            ( Model key url (Failure pathNotFound) dummySeed defaultPath
                            , Cmd.none
                            )

                        Just path ->
                            seedInitialize url key path query.seed


seedInitialize : Url -> Nav.Key -> Path -> Maybe Int -> ( Model, Cmd Msg )
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


jsonDecoder : Decoder Novel.Tree
jsonDecoder =
    JD.array
        (JD.map2 Novel.Node
            (JD.field "c" (JD.nullable JD.string))
            (JD.field "n" (JD.array JD.int))
        )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotTime Time.Posix
    | GotJson (Result Http.Error Novel.Tree)


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
            case urlToRoute url of
                Nothing ->
                    ( { model | url = url, state = Failure "お探しのページは見つかりませんでした。" }, Cmd.none )

                Just (Top query) ->
                    case ( query.path, query.seed ) of
                        ( Nothing, Nothing ) ->
                            ( { model | url = url, seed = model.seed + 1 }, Cmd.none )

                        ( Just path, Nothing ) ->
                            case Path.fromString path of
                                Nothing ->
                                    ( { model | url = url, state = Failure pathNotFound }, Cmd.none )

                                Just pathList ->
                                    ( { model | url = url, path = pathList, seed = model.seed + 1 }, Cmd.none )

                        ( Nothing, Just seed ) ->
                            ( { model | url = url, seed = seed }, Cmd.none )

                        ( Just path, Just seed ) ->
                            case Path.fromString path of
                                Nothing ->
                                    ( { model | url = url, state = Failure pathNotFound }, Cmd.none )

                                Just pathList ->
                                    ( { model | url = url, path = pathList, seed = seed }, Cmd.none )

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
                div [ class "error" ] [ text errorMessage ]

            Loading ->
                div [ class "loading" ] [ text "loading..." ]

            Success novelTree ->
                randomMazeHtml model novelTree
        ]
    }


randomMaze : Model -> Novel.Tree -> Maybe ( Maze, String )
randomMaze model novelTree =
    let
        random =
            Random.initialSeed model.seed
    in
    Novel.select random model.path novelTree |> Maybe.map (novelToMaze random novelTree)


defaultArea : Maze.Area
defaultArea =
    { top = Just 60
    , right = Just 30
    , bottom = Just 0
    , left = Just -30
    }


novelToMaze : Random.Seed -> Novel.Tree -> ( String, Path ) -> ( Maze, String )
novelToMaze random novelTree ( novel, novelPath ) =
    let
        chooser =
            Maze.randomChooser random

        oneWayMaze =
            Maze.makeExit chooser novel defaultArea novelPath

        forks =
            Path.toForks novelPath

        area =
            Maze.getArea oneWayMaze

        completeMaze =
            makeAllBranch random novelTree area forks oneWayMaze

        pathString =
            Path.toString novelPath
    in
    ( completeMaze, pathString )


makeAllBranch : Random.Seed -> Novel.Tree -> Maze.Area -> Path.Forks -> Maze -> Maze
makeAllBranch random novelTree area forks maze =
    if Set.isEmpty forks then
        maze

    else
        let
            ( nextPath, nextRandom ) =
                choosePathFromForks random forks

            chooser =
                Maze.randomChooser nextRandom

            result =
                Novel.select nextRandom nextPath novelTree
        in
        case result of
            Nothing ->
                maze

            Just ( nextNovel, completePath ) ->
                let
                    newForks =
                        Set.union (Set.remove nextPath forks) (Path.betweenForks nextPath completePath)

                    branch =
                        Maze.Branch nextPath completePath

                    newMaze =
                        Maze.addBranch chooser nextNovel area branch maze
                in
                makeAllBranch nextRandom novelTree area newForks newMaze


choosePathFromForks : Random.Seed -> Path.Forks -> ( Path, Random.Seed )
choosePathFromForks random forks =
    let
        size =
            Set.size forks

        generator =
            Random.int 0 (size - 1)

        ( index, nextRandom ) =
            Random.step generator random

        path =
            Set.toList forks |> Util.getNth index |> Maybe.withDefault []
    in
    ( path, nextRandom )


randomMazeHtml : Model -> Novel.Tree -> Html msg
randomMazeHtml model novelTree =
    case randomMaze model novelTree of
        Nothing ->
            div [ class "error" ] [ text pathNotFound ]

        Just ( maze, pathString ) ->
            let
                area =
                    Maze.getArea maze
            in
            div [ class "wrapper" ]
                [ header [ class "header" ]
                    [ h1 [] [ a [ href "/" ] [ text "迷路小説" ] ]
                    , ul [ id "link" ]
                        [ li [] [ a [ href "http://blog.livedoor.jp/kensaku_gokuraku/" ] [ text "ブログ" ] ]
                        , li [] [ a [ href "https://twitter.com/tannakaken//" ] [ text "Twitter" ] ]
                        , li [] [ a [ href "https://tannakaken.xyz/gallery/" ] [ text "ギャラリー" ] ]
                        , li [] [ a [ href "https://gallery.tannakaken.xyz/feedback/" ] [ text "フィードバック" ] ]
                        ]
                    ]
                , article [ class "main" ] [ div [ class "maze" ] <| mazeRows area maze ]
                , div [ class "bookmark" ] [ p [] [ text "ブックマーク用URL:", seedLink model ] ]
                , div [ class "explanation" ]
                    [ p [] [ text "リロードするたびに迷路は再生成されます。お気に入りの迷路が現れた場合は、このブックマーク用のURLをクリックしてからブックマークしてください。" ]
                    , p [] [ text "分かれ道や行き止まりはリンクになっています。クリックすると、その分岐がゴールへと繋がる迷路が生成されます。" ]
                    , p [] [ text "迷路の生成に時間がかかる場合がありますので、お待ちください。" ]
                    , p []
                        [ text "このページは迷路自動生成アルゴリズムを読んで思いつきました。Elmのコードは"
                        , a [ href "https://github.com/tannakaken/mazenovel" ] [ text "github" ]
                        , text "にあります。"
                        ]
                    ]
                , footer [ class "footer" ]
                    [ div []
                        [ p [] [ text "作者:", a [ href "https://tannakaken.xyz/" ] [ text "淡中 圏(Tannakian Cat)" ] ]
                        , p [] [ text "所属サークル:", a [ href "https://forcing.nagoya/" ] [ text "The dark side of forcing" ] ]
                        , address [] [ text "連絡先:", a [ href "mailto:tannakaken@gmail.com" ] [ text "tannakaken@gmail.com" ] ]
                        ]
                    , div []
                        [ dl [ id "document-history" ]
                            [ dt [ id "first-published" ] [ text "First Published:" ]
                            , dd [] [ time [] [ text "12/05/2022 23:00:00" ] ]
                            ]
                        ]
                    , div [ id "copyleft" ]
                        [ p [] [ text "@copyleft; All Wrongs Reversed" ] ]
                    ]
                ]


seedLink : Model -> Html msg
seedLink model =
    let
        link =
            urlForBookmark model.seed model.path model.url
    in
    a [ href link ] [ text link ]


mazeRows : Area -> Maze -> List (Html msg)
mazeRows area maze =
    mazeRowsAux area maze (Maybe.withDefault 0 area.top + 1) []


mazeRowsAux : Area -> Maze -> Int -> List (Html msg) -> List (Html msg)
mazeRowsAux area maze row acc =
    if row < (Maybe.withDefault 0 area.bottom - 1) then
        acc

    else
        mazeRowsAux area maze (row - 1) (acc ++ [ div [ class "row" ] (mazeColumns area maze row) ])


mazeColumns : Area -> Maze -> Int -> List (Html msg)
mazeColumns area maze row =
    mazeColumnsAux area maze row (Maybe.withDefault 0 area.left - 1) []


mazeColumnsAux : Area -> Maze -> Int -> Int -> List (Html msg) -> List (Html msg)
mazeColumnsAux area maze row column acc =
    if column > (Maybe.withDefault 0 area.right + 1) then
        acc

    else
        mazeColumnsAux area maze row (column + 1) (acc ++ [ mazeCoordinates ( column, row ) maze ])


mazeCoordinates : Coordinates -> Maze -> Html msg
mazeCoordinates coordinates maze =
    let
        char =
            Maze.getChar coordinates maze

        kind =
            Maze.getKind coordinates maze
    in
    case kind of
        Maze.Wall ->
            span [ class "wall" ] [ charToText char ]

        Maze.Space ->
            span [ class "space" ] [ charToText char ]

        Maze.Start ->
            span [ class "start" ] [ charToText char ]

        Maze.Fork _ ->
            span [ class "fork" ] [ a [ href <| String.append "/?path=" <| Path.toString <| Maze.getForkPath coordinates maze ] [ charToText char ] ]


charToText : Char -> Html msg
charToText =
    String.fromChar >> text
