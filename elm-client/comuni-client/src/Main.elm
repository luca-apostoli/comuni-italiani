module Main exposing (Model, Msg(..), init, main, parseResponse, showComune, showComuni, update, view)

import Browser
import Generated.ComuniApi exposing (Comune, getComuni)
import Html exposing (Html, b, div, fieldset, form, h1, h2, input, text, button)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onInput, onClick)
import Http



---- MODEL ----


type alias Model =
    { query : String
    , pos : Maybe Int
    , limit : Maybe Int
    , comuni : Result String (List Comune)
    }


init : ( Model, Cmd Msg )
init =
    ( { query = "", pos = Nothing, limit = Just 25, comuni = Ok [] }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateContent String
    | UpdateComuni (List Comune)
    | AddPosition (Maybe Int)
    | RequestError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateContent newContent ->
            ( { model | query = newContent }, getComuni (Just newContent) model.pos model.limit parseResponse )

        UpdateComuni comuni ->
            ( { model | comuni =  applyComuni model.comuni comuni }, Cmd.none )

        AddPosition Nothing -> ( model, Cmd.none )

        AddPosition (Just pos) ->
            ( { model | pos = Just <| pos + Maybe.withDefault 0 model.pos }, getComuni (Just model.query) (Just pos) model.limit parseResponse)

        RequestError err ->
            ( { model | comuni = Err err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


applyComuni : Result String (List Comune) -> List Comune -> Result String (List Comune)
applyComuni c1 c2 = case c1 of
                    Err _ -> Ok c2
                    Ok listCom1 -> Ok <| listCom1 ++ c2

parseResponse : Result Http.Error (List Comune) -> Msg
parseResponse resp =
    case resp of
        Ok comuni ->
            UpdateComuni comuni

        _ ->
            RequestError "Non ci sono risultati per la richiesta"


---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "pure-g" ]
        [ h1 [ class "pure-u-1" ] [ text "Ricerca comuni italiani" ]
        , div [ class "pure-u-1-3" ] []
        , form [ class "pure-form pure-form-aligned pure-u-1-3" ]
            [ fieldset []
                [ div [ class "pure-control-group" ]
                    [ input [ placeholder "Query", value model.query, onInput UpdateContent, style "width" "100%" ] []
                    ]
                ]
            ]
        , showComuni model.limit model.comuni
        ]


showComuni : Maybe Int -> Result String (List Comune) -> Html Msg
showComuni posToAdd res =
    case res of
        Err err ->
            div []
                [ h2 [] [ text err ]
                ]

        Ok comuni ->
            if List.length comuni > 0 then
                div [] [
                    div [ class "pure-u-1" ] <| List.map showComune comuni
                    , div [ class "pure-u-1" ] [
                        button [ onClick <| AddPosition posToAdd ] [ text "Carica altri risultati" ]
                    ] 
                ]
            else
                div [ class "pure-u-1 " ] [ text "cerca un comune per Nome, CAP, Regione o Provincia" ]


showComune : Comune -> Html Msg
showComune c =
    div [ class "pure-g m-box" ]
        [ b [ class "pure-u-1" ] [ text c.nome ]
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
