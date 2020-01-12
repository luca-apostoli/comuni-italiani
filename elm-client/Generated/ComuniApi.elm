module Generated.ComuniApi exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias Comune  =
   { nome: String
   , codice: String
   , zona: Zona
   , regione: Regione
   , provincia: Provincia
   , sigla: String
   , codiceCatastale: String
   , cap: (List String)
   , popolazione: Int
   }

jsonDecComune : Json.Decode.Decoder ( Comune )
jsonDecComune =
   Json.Decode.succeed (\pnome pcodice pzona pregione pprovincia psigla pcodiceCatastale pcap ppopolazione -> {nome = pnome, codice = pcodice, zona = pzona, regione = pregione, provincia = pprovincia, sigla = psigla, codiceCatastale = pcodiceCatastale, cap = pcap, popolazione = ppopolazione})
   |> required "nome" (Json.Decode.string)
   |> required "codice" (Json.Decode.string)
   |> required "zona" (jsonDecZona)
   |> required "regione" (jsonDecRegione)
   |> required "provincia" (jsonDecProvincia)
   |> required "sigla" (Json.Decode.string)
   |> required "codiceCatastale" (Json.Decode.string)
   |> required "cap" (Json.Decode.list (Json.Decode.string))
   |> required "popolazione" (Json.Decode.int)

jsonEncComune : Comune -> Value
jsonEncComune  val =
   Json.Encode.object
   [ ("nome", Json.Encode.string val.nome)
   , ("codice", Json.Encode.string val.codice)
   , ("zona", jsonEncZona val.zona)
   , ("regione", jsonEncRegione val.regione)
   , ("provincia", jsonEncProvincia val.provincia)
   , ("sigla", Json.Encode.string val.sigla)
   , ("codiceCatastale", Json.Encode.string val.codiceCatastale)
   , ("cap", (Json.Encode.list Json.Encode.string) val.cap)
   , ("popolazione", Json.Encode.int val.popolazione)
   ]



type alias Zona  =
   { z_codice: String
   , z_nome: String
   }

jsonDecZona : Json.Decode.Decoder ( Zona )
jsonDecZona =
   Json.Decode.succeed (\pz_codice pz_nome -> {z_codice = pz_codice, z_nome = pz_nome})
   |> required "z_codice" (Json.Decode.string)
   |> required "z_nome" (Json.Decode.string)

jsonEncZona : Zona -> Value
jsonEncZona  val =
   Json.Encode.object
   [ ("z_codice", Json.Encode.string val.z_codice)
   , ("z_nome", Json.Encode.string val.z_nome)
   ]



type alias Regione  =
   { r_codice: String
   , r_nome: String
   }

jsonDecRegione : Json.Decode.Decoder ( Regione )
jsonDecRegione =
   Json.Decode.succeed (\pr_codice pr_nome -> {r_codice = pr_codice, r_nome = pr_nome})
   |> required "r_codice" (Json.Decode.string)
   |> required "r_nome" (Json.Decode.string)

jsonEncRegione : Regione -> Value
jsonEncRegione  val =
   Json.Encode.object
   [ ("r_codice", Json.Encode.string val.r_codice)
   , ("r_nome", Json.Encode.string val.r_nome)
   ]



type alias Provincia  =
   { p_codice: String
   , p_nome: String
   }

jsonDecProvincia : Json.Decode.Decoder ( Provincia )
jsonDecProvincia =
   Json.Decode.succeed (\pp_codice pp_nome -> {p_codice = pp_codice, p_nome = pp_nome})
   |> required "p_codice" (Json.Decode.string)
   |> required "p_nome" (Json.Decode.string)

jsonEncProvincia : Provincia -> Value
jsonEncProvincia  val =
   Json.Encode.object
   [ ("p_codice", Json.Encode.string val.p_codice)
   , ("p_nome", Json.Encode.string val.p_nome)
   ]


getComuni : (Maybe String) -> (Result Http.Error  ((List Comune))  -> msg) -> Cmd msg
getComuni query_q toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_q
                    |> Maybe.map (Url.Builder.string "q") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "comuni"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecComune))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getComuneByCodice : String -> (Result Http.Error  (Comune)  -> msg) -> Cmd msg
getComuneByCodice capture_codice toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin ""
                    [ "comune"
                    , capture_codice
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecComune
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
