module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Route exposing (Route)
import Url
import Url.Builder



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

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

-- MODEL


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | TopPage
    | UserPage (List Repo)
    | RepoPage (List Issue)
    | ErrorPage Http.Error

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    -- 後に画面遷移で使うためのキーを Model に持たせておく
    Model key TopPage
        -- はじめてページを訪れたときにページの初期化を行う
        |> goTo (Route.parse url)


-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | Loaded (Result Http.Error Page)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- （１）画面遷移のリクエストを受けたとき
        LinkClicked urlRequest ->
            -- Debug.todo "リンクがクリックされたときの挙動を実装する"
            case urlRequest of
                -- 内部リンクならブラウザのURLを更新する
                -- (SPAなので実際に表示するページはindex.htmlのまま)
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )
                -- 外部リンクなら通常の画面遷移を行う
                Browser.External href ->
                    ( model, Nav.load href )

        -- （２）ブラウザのURLが変更されたとき
        UrlChanged url ->
            -- ページの初期化処理をヘルパー関数(goTo関数)に移譲
            goTo (Route.parse url) model
        
        -- （３）HTTPリクエストが返ってきたとき（ページがロードされたとき）
        Loaded result ->
            -- Debug.todo "データが取得された後の挙動を実装する"
            ( { model
                | page =
                    case result of
                        Ok page ->
                            Page
                        Err e ->
                            -- 失敗したときはエラー用のページ
                            ErrorPage e
              }
            , Cmd.none
            )

{- パス（URL）に応じて各ページを初期化する -}
goTo : Maybe Route -> Model -> ( Model, Cmd Msg ) 
goTo maybeRoute model =
    case maybeRoute of
        -- URLに該当するURLがなかった場合はNot Foundページ
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        -- Route.Topだった場合はトップページ
        Just Route.Top ->
            ( { model | page = TopPage }, Cmd.none )

        -- Route.Userだった場合
        Just (Route.User userName) ->
            -- Debug.todo "ページを初期化するためにデータを取得する"
            ( model
            , Http.get
                { url =
                    Url.Builder.crossOrigin "https://api.github.com"
                        [ "users", userName, "repos"]
                        []
                , expect =
                    Http.expectJson
                        (Result.map UserPage >> Loaded)
                        reposDecoder
                }
            )

        -- Route.Repoだった場合
        Just (Route.Repo userName projectName) ->
            --Debug.todo "ページを初期化するためにデータを取得する"
            ( model
            , Http.get
                { url =
                    Url.Builder.crossOrigin "https://api.github.com"
                        [ "repos", userName, projectName, "issues"]
                        []
                , expect =
                    Http.expectJson
                        (Result.map RepoPage >> Loaded)
                        issuesDecoder
                }
            )


-- GITHUB


type alias Repo =
    { name: String
    , description: String
    , language: String
    , owner: String
    , fork: Int
    , star: Int
    , watch: Int
    }


type alias Issue =
    { number: Int
    , title: String
    , state: String
    }

reposDecoder : Decoder (List Repo)
reposDecoder =
    D.list repoDecoder

repoDecoder : Decoder Repo
repoDecoder =
    D.map7 Repo
        (D.field "name" D.string)
        (D.field "description" D.string)
        (D.maybe (D.field "language" D.string))
        (D.at [ "owner", "login" ] D.string)
        (D.field "forks_count" D.int)
        (D.field "stargazers_count" D.int)
        (D.field "watchers_count" D.int)

issuesDecoder : Decoder (List Issue)
issuesDecoder =
    D.list issueDecoder

issueDecoder : Decoder Issue
issueDecoder =
    D.map3 Issue
        (D.field "number" D.int)
        (D.field "title" D.string)
        (D.field "state" D.string)

-- VIEW


view : Model -> Browser.Document Msg
view model =

