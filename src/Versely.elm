port module Versely exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, placeholder, type_, disabled, value, id, autocomplete)
import Html.Events exposing (onInput, onSubmit, onFocus, onBlur, on, keyCode)
import Browser
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode exposing (Decoder, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Http
import Html.Events exposing (onClick)
import Browser.Dom as Dom exposing (focus)
import Task exposing (attempt)
import List.Extra as LE exposing (elemIndex, getAt)

type alias Model =
  { searchText : String
  , searching : Bool
  , searchHistory : List String
  , scripture : Maybe Scripture
  , error : Maybe Http.Error
  , promptVisible : Bool
  }

type alias Scripture = 
  { reference : String
  , text : String
  , translation_name : String
  }

initialModel : Model
initialModel =
  { searchText = ""
  , searching = False
  , searchHistory = []
  -- , scripture = Nothing
  , scripture = (Just { text = "Now there was a man of the Pharisees named Nicodemus, a ruler of the Jews.", reference = "John 3:1", translation_name = "World English Bible" })
  , error = Nothing
  , promptVisible = False
  }

allBooks : List String
allBooks =
  ["Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy", "Joshua", "Judges", "Ruth", "1 Samuel", "2 Samuel", "1 Kings", "2 Kings", "1 Chronicles", "2 Chronicles", "Ezra", "Nehemiah", "Esther", "Job", "Psalms", "Proverbs", "Ecclesiastes", "Song of Solomon", "Isaiah", "Jeremiah", "Lamentations", "Ezekiel", "Daniel", "Hosea", "Joel", "Amos", "Obadiah", "Jonah", "Micah", "Nahum", "Habakkuk", "Zephaniah", "Haggai", "Zechariah", "Malachi", "Matthew", "Mark", "Luke", "John", "Acts", "Romans", "1 Corinthians", "2 Corinthians", "Galatians", "Ephesians", "Philippians", "Colossians", "1 Thessalonians", "2 Thessalonians", "1 Timothy", "2 Timothy", "Titus", "Philemon", "Hebrews", "James", "1 Peter", "2 Peter", "1 John", "2 John", "3 John", "Jude", "Revelation"]

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
  | Search
  | LoadScripture (Result Http.Error Scripture)
  | TogglePrompt Bool
  | LoadPrompt String
  | FocusOn (Result Dom.Error ())

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    UpdateSearchBox text -> 
      (
        { model | searchText = text }
        , Cmd.none
      )
    Search ->
      (
        { model | searchText = "", scripture = Nothing, error = Nothing, searching = True, promptVisible = False, searchHistory = (LE.unique (model.searchText :: model.searchHistory)) }
        , fetchScripture model.searchText
      )
    LoadScripture (Ok scripture) ->
      (
        { model | scripture = Just scripture, searching = False }
        , Cmd.none
      )
    LoadScripture (Err error) ->
      (
        { model | error = Just error, searching = False }
        , Cmd.none
      )
    TogglePrompt show ->
      (
        { model | promptVisible = show }
        , Cmd.none
      )
    LoadPrompt prompt ->
      (
        { model | searchText = prompt }
        , Dom.focus "search-box" |> Task.attempt FocusOn
      )
    FocusOn result ->
      case result of
        Err (Dom.NotFound _) ->
          ( model, Cmd.none )
        Ok () ->
          ( model, Cmd.none )

view : Model -> Html Msg
view model =
  div []
    [ div [ class "header" ]
      [
        h1 [] [text "Versely"]
      ]
    , div [ class "body" ]
      [ viewSearchBox model
      , viewPrompt model
      , viewRecentSearches model
      , viewResult model
      ]
    ]

viewSearchBox : Model -> Html Msg
viewSearchBox model =
  form [ onSubmit Search, class "body-item", autocomplete False ]
       [
         input 
         [ type_ "text"
         , id "search-box"
         , placeholder "Search for a verse..."
         , value model.searchText
         , onInput UpdateSearchBox
         , onFocus (TogglePrompt True)
         , onClick (TogglePrompt True)
         ]
         []
       , button 
         [ type_ "submit"
         , disabled (String.length model.searchText < 1)
         ]
         [ text "Search" ]
       ]

viewRecentSearches : Model -> Html Msg
viewRecentSearches model =
  div [ class "recent-searches" ]
      ( List.map viewRecentSearch model.searchHistory )

viewRecentSearch : String -> Html Msg
viewRecentSearch search =
  div [ class "search-link", onClick (LoadPrompt search)]
      [ text search ]

viewPrompt : Model -> Html Msg
viewPrompt model =
  if model.promptVisible && (List.length (matchingBooks model.searchText allBooks)) > 0 then
    div [ class "body-item" ]
        [ div [ class "prompt" ]
              [ div [ class "prompt-header" ]
                    [ span [] []
                    , h3 [] [ text "Books" ]
                    , span
                      [ onClick (TogglePrompt False)
                      , class "close-icon"
                      ]
                      [ text "X" ]
                    ]
              , div []
                ( 
                  allBooks
                  |> matchingBooks model.searchText
                  |> List.map viewBookPrompt
                )
              ]
        ]
  else
    div [] []

matchingBooks : String -> List String -> List String
matchingBooks searchText books =
  List.filter ( \book -> 
                  String.contains (String.toLower searchText) (String.toLower book)
              ) books

viewBookPrompt : String -> Html Msg
viewBookPrompt book =
  div [ class "prompt-result" 
      , onClick (LoadPrompt (book ++ " "))
      ]
      [ text book ]


viewResult : Model -> Html Msg
viewResult model =
  case model.error of
    Just _ ->
      div []
          [ text "An error occured fetching the verse"]

    Nothing ->
      if model.searching then
        div [ class "spinner-wrapper" ]
            [
              div [ class "search-spinner" ] []
            ]
      else
        viewScripture model.scripture

viewScripture : Maybe Scripture -> Html Msg
viewScripture maybeScripture =
    case maybeScripture of
      Just scripture ->
        div [ class "scripture-card" ]
            [ div []
                  [ text scripture.text ]
            , br [] []
            , small []
                  [ text (scripture.reference ++ " | " ++ scripture.translation_name) ]
            ] 
      Nothing ->
        div [] []

port setStorage : List String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions _ =
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