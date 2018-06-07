module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


(=>) : a -> b -> ( a, b )
(=>) =
    (,)



---- MODEL ----


type alias Model =
    ()


init : ( Model, Cmd Never )
init =
    ( (), Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Never -> Model -> ( Model, Cmd Never )
update msg model =
    model ! []



---- VIEW ----


view : Model -> Html Never
view model =
    div [ class "main" ]
        [ img [ class "logo", src "haskell.png" ] []
        , div [ class "links" ] (List.map renderLink links)
        ]


renderLink : ( String, String ) -> Html Never
renderLink ( site, url ) =
    a
        [ class "link", href url ]
        [ text site ]


links =
    [ "GitHub" => "https://github.com/jjant"
    , "Twitter" => "https://twitter.com/_jjant"
    , "LinkedIn" => "https://www.linkedin.com/in/julianantonielli"
    ]



---- PROGRAM ----


main : Program Never Model Never
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
