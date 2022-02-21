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
  { searchText : String
  , scripture : Maybe Scripture
  , error : Maybe Http.Error
  }

type alias Scripture = 
  { reference : String
  , text : String
  , translation_name : String
  }

initialModel : Model
initialModel =
  { searchText = ""
  -- , scripture = Nothing
  , scripture = (Just { text = "Now there was a man of the Pharisees named Nicodemus, a ruler of the Jews.", reference = "John 3:1", translation_name = "World English Bible" })
  , error = Nothing
  }

scriptureDecoder : Decoder Scripture
scriptureDecoder =
  succeed Scripture
    |> required "reference" string
    |> required "text" string
    |> required "translation_name" string


fetchScripture : String -> Cmd Msg
fetchScripture search =
  Http.get
    {
      url = "https://bible-api.com/" ++ search
      , expect  = Http.expectJson LoadScripture scriptureDecoder
    }

type Msg 
  = UpdateSearchBox String
  | Search String
  | LoadScripture (Result Http.Error Scripture)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateSearchBox text -> 
      (
        { model | searchText = text }
        , Cmd.none
      )

    Search text ->
      (
        { model | searchText = "", error = Nothing }
        , fetchScripture model.searchText
      )

    LoadScripture (Ok scripture) ->
      (
        { model | scripture = Just scripture }
        , Cmd.none
      )

    LoadScripture (Err error) ->
      (
        { model | error = Just error }
        , Cmd.none
      )

view : Model -> Html Msg
view model =
  div []
    [ div [ class "header" ]
      [
        h1 [] [text "Versely"]
      ]
    , div [ class "body" ]
      [ viewSearchBox model
      , viewResult model
      ]
    ]

viewSearchBox : Model -> Html Msg
viewSearchBox model =
  div [ class "search" ]
  [ input 
    [ type_ "text"
    , placeholder "Search for a verse..."
    , value model.searchText
    , onInput UpdateSearchBox
    ]
    []
  , button 
    [ onClick (Search model.searchText)
    , disabled (String.length model.searchText < 1)
    ]
    [ text "Search" ]
  ]

viewResult : Model -> Html Msg
viewResult model =
  case model.error of
    Just error ->
      div []
          [ text "An error occured fetching the verse"]

    Nothing ->
      viewScripture model.scripture

viewScripture : Maybe Scripture -> Html Msg
viewScripture maybeScripture =
    case maybeScripture of
      Just scripture ->
        div []
            [ div []
                  [ text scripture.text ]
            , br [] []
            , small []
                  [ text (scripture.reference ++ " | " ++ scripture.translation_name) ]
            ] 
      Nothing ->
        div [] []

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