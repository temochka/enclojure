module Docs exposing (..)

import Browser
import Browser.Navigation
import Element exposing (Element)
import Element.Font
import Element.Input
import Element.Keyed
import Enclojure
import File.Download
import Html
import Url exposing (Url)


type Page
    = Docs
    | Repl
    | NotFound


type alias DocsPage =
    { query : String
    , docs : List ( Enclojure.Doc, Enclojure.FnInfo )
    , matchingDocs : List ( Enclojure.Doc, Enclojure.FnInfo )
    }


type alias Model =
    { navigationKey : Browser.Navigation.Key
    , docsPage : DocsPage
    , currentPage : Page
    }


type DocsMessage
    = UpdateQuery String
    | Download


type Message
    = OnDocsPage DocsMessage
    | Nop
    | RequestUrl Browser.UrlRequest
    | RouteUrl Url


init : () -> Url -> Browser.Navigation.Key -> ( Model, Cmd Message )
init _ currentUrl navigationKey =
    let
        docs =
            Enclojure.documentation Enclojure.init
    in
    ( { docsPage = { query = "", docs = docs, matchingDocs = docs }
      , navigationKey = navigationKey
      , currentPage = routeUrl currentUrl
      }
    , Cmd.none
    )


symbolInfo : ( Enclojure.Doc, Enclojure.FnInfo ) -> Maybe ( String, Element Message )
symbolInfo ( docType, { name, doc, signatures } ) =
    Maybe.map2
        (\n d ->
            ( n
            , Element.column
                [ Element.spacing 10 ]
                [ Element.paragraph [ Element.Font.size 22, Element.Font.semiBold ] [ Element.text n ]
                , (case docType of
                    Enclojure.FunctionDoc ->
                        "function"

                    Enclojure.MacroDoc ->
                        "macro"

                    Enclojure.SpecialFormDoc ->
                        "special form"
                  )
                    |> Element.text
                , Element.row [ Element.spacing 10 ]
                    [ Element.el [ Element.alignTop ] (Element.text "Usage:")
                    , signatures
                        |> List.map
                            (\s ->
                                Element.el
                                    [ Element.Font.family [ Element.Font.monospace ] ]
                                    (Element.text ("(" ++ String.join " " (n :: s) ++ ")"))
                            )
                        |> Element.column []
                    ]
                , Element.paragraph [] [ Element.text d ]
                ]
            )
        )
        name
        doc


symbolInfoMd : ( Enclojure.Doc, Enclojure.FnInfo ) -> Maybe String
symbolInfoMd ( docType, { name, doc, signatures } ) =
    Maybe.map2
        (\n d ->
            [ "## " ++ n
            , case docType of
                Enclojure.FunctionDoc ->
                    "`(function)`"

                Enclojure.MacroDoc ->
                    "`(macro)`"

                Enclojure.SpecialFormDoc ->
                    "`(special form)`"
            , [ "Usage:"
              , [ "```"
                , signatures
                    |> List.map
                        (\s ->
                            "(" ++ String.join " " (n :: s) ++ ")"
                        )
                    |> String.join "\n"
                , "```"
                ]
                    |> String.join "\n"
              ]
                |> String.join "\n\n"
            , d
            ]
                |> String.join "\n\n"
        )
        name
        doc


viewDocs : DocsPage -> Element Message
viewDocs model =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ Element.Input.button []
                { onPress = Just (OnDocsPage Download)
                , label = Element.text "Download as Markdown"
                }
            , Element.Input.text
                [ Element.width Element.fill ]
                { onChange = UpdateQuery >> OnDocsPage
                , text = model.query
                , placeholder = Just (Element.Input.placeholder [] (Element.text "Filter docs..."))
                , label = Element.Input.labelHidden "Search docs"
                }
            ]
        , Element.Keyed.column [ Element.width Element.fill, Element.spacing 20 ] <|
            List.filterMap symbolInfo model.matchingDocs
        ]


view : Model -> Browser.Document Message
view model =
    { title = "Enclojure"
    , body =
        [ Element.column
            [ Element.padding 20
            , Element.spacing 25
            , Element.width Element.fill
            ]
            [ Element.row [ Element.spacing 25, Element.width Element.fill ]
                [ Element.el [ Element.Font.size 25, Element.Font.semiBold ] (Element.text "Enclojure API")
                , Element.link [] { url = "/docs", label = Element.text "Docs" }
                , Element.link [] { url = "/repl", label = Element.text "REPL" }
                , Element.link [] { url = "https://github.com/temochka/enclojure", label = Element.text "GitHub" }
                ]
            , case model.currentPage of
                Docs ->
                    viewDocs model.docsPage

                Repl ->
                    Element.none

                NotFound ->
                    Element.el
                        [ Element.width Element.fill, Element.height Element.fill ]
                        (Element.column [ Element.centerX, Element.centerY ]
                            [ Element.text "Page Not Found"
                            , Element.link [] { url = "/docs", label = Element.text "Return to Docs" }
                            ]
                        )
            ]
            |> Element.layout []
        ]
    }


matchDocs : String -> List ( Enclojure.Doc, Enclojure.FnInfo ) -> List ( Enclojure.Doc, Enclojure.FnInfo )
matchDocs query =
    List.filter
        (\( _, { name } ) ->
            case name of
                Just n ->
                    n |> String.toLower |> String.contains query

                Nothing ->
                    False
        )


updateDocs : DocsMessage -> DocsPage -> ( DocsPage, Cmd Message )
updateDocs msg model =
    case msg of
        UpdateQuery query ->
            ( { model
                | query = query
                , matchingDocs = matchDocs query model.docs
              }
            , Cmd.none
            )

        Download ->
            let
                markdown =
                    "# Enclojure API\n\n" ++ (model.docs |> List.filterMap symbolInfoMd |> String.join "\n\n")
            in
            ( model
            , File.Download.string "API.md" "text/markdown" markdown
            )


routeUrl : Url -> Page
routeUrl url =
    case url.path of
        "/docs" ->
            Docs

        "/repl" ->
            Repl

        _ ->
            NotFound


update : Message -> Model -> ( Model, Cmd Message )
update msg model =
    case msg of
        RequestUrl urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navigationKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        RouteUrl url ->
            ( { model | currentPage = routeUrl url }, Cmd.none )

        OnDocsPage docsMessage ->
            updateDocs docsMessage model.docsPage
                |> Tuple.mapFirst (\m -> { model | docsPage = m })

        Nop ->
            ( model, Cmd.none )


main : Program () Model Message
main =
    Browser.application
        { init = init
        , subscriptions = always Sub.none
        , view = view
        , update = update
        , onUrlRequest = RequestUrl
        , onUrlChange = RouteUrl
        }
