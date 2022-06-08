module Docs exposing (..)

import Browser
import Element exposing (Element)
import Element.Font
import Enclojure
import Html


type alias Model =
    List ( Enclojure.Doc, Enclojure.FnInfo )


type alias Message =
    Never


init : Model
init =
    Enclojure.documentation Enclojure.init


symbolInfo : ( Enclojure.Doc, Enclojure.FnInfo ) -> Element Message
symbolInfo ( docType, { name, doc, signatures } ) =
    Maybe.map2
        (\n d ->
            Element.column
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
                                Element.el [ Element.Font.family [ Element.Font.monospace ] ] <| Element.text ("(" ++ String.join " " (n :: s) ++ ")")
                            )
                        |> Element.column []
                    ]
                , Element.paragraph [] [ Element.text d ]
                ]
        )
        name
        doc
        |> Maybe.withDefault Element.none


view : Model -> Html.Html Message
view model =
    model
        |> List.map symbolInfo
        |> Element.column [ Element.padding 20, Element.spacing 20 ]
        |> Element.layout []


update : Message -> Model -> Model
update _ model =
    model


main : Program () Model Message
main =
    Browser.sandbox { init = init, view = view, update = update }
