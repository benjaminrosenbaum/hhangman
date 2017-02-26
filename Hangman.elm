-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

module Hangman exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Char
import Http
import RandomWords 
import Drawing
import Set exposing (Set)




main =
  Html.program
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL

type alias Guess = Char 

type alias Model = 
  { word : String,
    error : Maybe String
  , guesses : Set Guess
  }

init : (Model, Cmd Msg)
init = (Model "" Nothing Set.empty, Cmd.none)

wordChars : Model -> Set Char
wordChars m = String.toList m.word |> Set.fromList

mistakes : Model -> Set Guess
mistakes m =  Set.diff m.guesses (wordChars m) 

tooManyMistakes : Model -> Bool
tooManyMistakes m = Drawing.maxMistakes <= Set.size (mistakes m)


-- UPDATE


type Msg
  = RefreshWord
  | NewWord (Result Http.Error (List String))
  | NewGuess String


refreshWord : (List String) -> Model -> Model
refreshWord newWords model =
  let word = RandomWords.selectWord newWords
  in Model word Nothing Set.empty

addGuess : String -> Model -> Model
addGuess guess model = 
  let g = String.toLower guess
      old = String.fromList (Set.toList model.guesses) 
      updated = if String.contains g old then old else old ++ g
      newGuesses = String.toList updated |> Set.fromList
  in Model model.word model.error newGuesses


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RefreshWord ->
      (model, RandomWords.getRandomWords NewWord)

    NewWord (Ok newWords) ->
      (refreshWord newWords model, Cmd.none)    

    NewWord (Err err) ->
      let error = Just (toString err)
      in (Model "" error Set.empty, Cmd.none)

    NewGuess guess ->
      (addGuess guess model, Cmd.none)


-- VIEW

board : Model -> String
board m = m.word
          |> String.toList
          |> List.map (\c -> if Set.member c m.guesses then Char.toUpper c else '_')
          |> List.intersperse ' '
          |> String.fromList

picture : Model -> String
picture m = mistakes m 
            |> Set.size
            |> Drawing.drawAtStep

wrongSoFar : Model -> String
wrongSoFar m = mistakes m 
               |> Set.map Char.toUpper
               |> Set.toList
               |> List.sort
               |> List.intersperse ','
               |> String.fromList

  


view : Model -> Html Msg
view model =
  case model.error of
    Nothing ->
      div []
        [ div [] [pre [] [text (board model)]]
          , div [] [pre [] [text (picture model)]]
          , div [] [pre [] [text ("Wrong guesses so far: " ++ (wrongSoFar model))]]
          , h2 [] [text model.word]
          , input [ placeholder "type your guesses here", onInput NewGuess, style [], value ""] []
          , button [ onClick RefreshWord ] [ text ">>Fetch A New Secret Word<<" ]
        ]

    Just errorMessage ->
      div []
        [ h2 [] [text ("Error retrieving secret word: " ++ errorMessage)]
          , button [ onClick RefreshWord ] [ text ">>Try Again<<" ]
        ]  



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



