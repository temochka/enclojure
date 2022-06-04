module Docs exposing (..)

import Browser
import Enclojure
import Html


type alias Model =
    String


type alias Message =
    Never


init : Model
init =
    "Hello"


view : Model -> Html.Html Message
view model =
    Html.text model


update : Message -> Model -> Model
update _ model =
    model


main : Program () Model Message
main =
    Browser.sandbox { init = init, view = view, update = update }
