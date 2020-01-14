module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, form, fieldset, input, b, h2)
import Html.Attributes exposing (src, placeholder, value, class, style)
import Html.Events exposing (onInput)
import Http
import Generated.ComuniApi exposing (Comune, getComuni)

---- MODEL ----


type alias Model =
    {
        query : String
    , comuni : Result String (List Comune)
    }


init : ( Model, Cmd Msg )
init =
    ( { query = "", comuni = Ok [] }, Cmd.none )

---- UPDATE ----

type Msg
    = NoOp
    | UpdateContent String
    | UpdateComuni (List Comune)
    | RequestError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateContent newContent -> ( { model | query = newContent }, getComuni (Just newContent) parseResponse)
        UpdateComuni comuni -> ( { model | comuni = Ok comuni }, Cmd.none)
        RequestError err -> ( { model | comuni = Err err }, Cmd.none)
        NoOp -> ( model, Cmd.none )


parseResponse : Result Http.Error (List Comune)  -> Msg
parseResponse resp = case resp of 
    (Ok comuni) -> UpdateComuni comuni
    _ -> RequestError "Non ci sono risultati per la richiesta"

---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "pure-g" ]
        [ h1 [ class "pure-u-1" ] [ text "Ricerca comuni italiani" ]
        , div [ class "pure-u-1-3" ] []
        , form [ class "pure-form pure-form-aligned pure-u-1-3"] [
            fieldset [] [
                div [class "pure-control-group"] [
                    input [placeholder "Query", value model.query, onInput UpdateContent, style "width" "100%"] []
                ]
            ]
          ]
        , showComuni model.comuni
        ]

showComuni : Result String (List Comune) -> Html Msg
showComuni res = case res of 
    Err err -> div [] [
            h2 [] [ text err ]
        ]
    Ok comuni -> div [ class "pure-u-1" ] <| List.map showComune comuni

showComune : Comune -> Html Msg
showComune c = div [ class "pure-g m-box" ] [
        b [ class "pure-u-1" ] [ text c.nome ]
        , div [ class "pure-u-1-4" ] [ text c.regione.r_nome ]
        , div [ class "pure-u-1-4" ] [ text c.provincia.p_nome ]
        , div [ class "pure-u-1-4" ] [ text c.codiceCatastale ]
    ]

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
