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

type Msg =
  UpdateSearchBox

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateSearchBox -> 
      (
        model,
        Cmd.none
      )

view : Model -> (Html Msg)
view model =
  text "Hello, World!"

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