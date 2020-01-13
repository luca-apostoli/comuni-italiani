module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img, input, b, h2)
import Html.Attributes exposing (src, placeholder, value)
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
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Ricerca comuni italiani" ]
        , div [] [
            input [placeholder "Query", value model.query, onInput UpdateContent] []  
          ]
        , showComuni model.comuni
        ]

showComuni : Result String (List Comune) -> Html Msg
showComuni res = case res of 
    Err err -> div [] [
            h2 [] [ text err ]
        ]
    Ok comuni -> div [] <| List.map showComune comuni

showComune : Comune -> Html Msg
showComune c = div [] [
        b [] [ text c.nome ]
        , div [] [ text c.regione.r_nome ]
        , div [] [ text c.provincia.p_nome ]
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
