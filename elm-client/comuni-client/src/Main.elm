module Main exposing (Model, Msg(..), init, main, parseResponse, showComune, showComuni, update, view)

import Browser
import Generated.ComuniApi exposing (Comune, ListaComuni, getComuni)
import Html exposing (Html, b, button, div, fieldset, form, h1, h2, input, text)
import Html.Attributes exposing (class, placeholder, style, value)
import Html.Events exposing (onClick, onInput)
import Http



---- MODEL ----


type alias Model =
    { query : String
    , pos : Maybe Int
    , limit : Maybe Int
    , listaComuni : Result String ListaComuni
    , loading : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { query = "", pos = Nothing, limit = Just 25, listaComuni = Ok <| ListaComuni [] 0, loading = False }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | UpdateContent String
    | UpdateComuni ListaComuni
    | AddPosition (Maybe Int)
    | RequestError String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateContent newContent ->
            ( { model | query = newContent, listaComuni = Ok <| ListaComuni [] 0, loading = True }, 
                if String.isEmpty newContent then Cmd.none
                else getComuni (Just newContent) model.pos model.limit parseResponse 
            )

        UpdateComuni listaComuni ->
            ( { model | listaComuni = Ok <| applyComuni model.listaComuni listaComuni, loading = False }, Cmd.none )

        AddPosition Nothing ->
            ( model, Cmd.none )

        AddPosition (Just pos) ->
            ( { model | pos = Just <| pos + Maybe.withDefault 0 model.pos, loading = True }, getComuni (Just model.query) (Just pos) model.limit parseResponse )

        RequestError err ->
            ( { model | listaComuni = Err err }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


applyComuni : Result String ListaComuni -> ListaComuni -> ListaComuni
applyComuni c1 c2 =
    case c1 of
        Err _ ->
            c2

        Ok listCom1 ->
            { c2 | comuni = listCom1.comuni ++ c2.comuni, totale = c2.totale }


parseResponse : Result Http.Error ListaComuni -> Msg
parseResponse resp =
    case resp of
        Ok listaComuni ->
            UpdateComuni listaComuni

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
        , showComuni model.limit model.listaComuni
        ]


showComuni : Maybe Int -> Result String ListaComuni -> Html Msg
showComuni posToAdd res =
    case res of
        Err err ->
            div []
                [ h2 [] [ text err ]
                ]

        Ok listaComuni ->
            if List.length listaComuni.comuni > 0 then
                div [ class "pure-u-1" ]
                    [ div [] <| List.map showComune listaComuni.comuni
                    , div [ class "pure-u-1-1" ]
                        [ if listaComuni.totale - List.length listaComuni.comuni > 0 then
                            div [ class "pure-control-group" ]
                                [ button [ onClick <| AddPosition posToAdd, class "pure-button pure-button-primary" ] [ text "Carica altri risultati" ]
                                ]

                          else
                            text <| "Hai caricato tutti i " ++ String.fromInt listaComuni.totale ++ " comuni corrispondenti alla ricerca"
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
