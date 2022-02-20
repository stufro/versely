module Versely exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, placeholder, src, type_, disabled, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Browser
import Array exposing (Array)
import Json.Decode exposing (Decoder, bool, decodeString, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Http

type alias Model =
  { searchText : String }

initialModel : Model
initialModel =
  Model ""

type Msg 
  = UpdateSearchBox String
  | Search String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateSearchBox text -> 
      (
        { model | searchText = text },
        Cmd.none
      )

    Search text ->
      (
        { model | searchText = "" },
        Cmd.none
      )

view : Model -> Html Msg
view model =
  div []
    [ div [ class "header" ]
      [
        h1 [] [text "Versely"]
      ]
    , div [ class "body" ]
      [
        input 
          [ type_ "text"
          , placeholder "Search for a verse..."
          , value model.searchText
          , onInput UpdateSearchBox
          ]
          []
        , button 
          [ onClick (Search model.searchText) ]
          [ text "Search" ]
      ]
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

init : () -> (Model, Cmd Msg)
init () =
  ( initialModel, Cmd.none )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }