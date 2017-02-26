
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

type GameResult = Undecided 
                  | Lost 
                  | Won  

type alias Guess = Char 

type alias Model = 
  {   word : String
    , error : Maybe String
    , guesses : Set Guess
    , result : GameResult
  }

init : (Model, Cmd Msg)
init = (Model "" Nothing Set.empty Undecided, Cmd.none)

wordChars : Model -> Set Char
wordChars m = String.toList m.word |> Set.fromList

mistakes : Model -> Set Guess
mistakes m =  Set.diff m.guesses (wordChars m) 

tooManyMistakes : Model -> Bool
tooManyMistakes m = Drawing.maxMistakes <= Set.size (mistakes m)

unguessedLetters : Model -> Set Guess
unguessedLetters m = Set.diff (wordChars m) m.guesses

result : Model -> GameResult
result m =  if tooManyMistakes m then Lost
            else if Set.isEmpty (unguessedLetters m) then Won
            else Undecided

-- UPDATE

type Msg
  = RefreshWord
  | NewWord (Result Http.Error (List String))
  | NewGuess String


refreshWord : (List String) -> Model -> Model
refreshWord newWords model =
  let word = RandomWords.selectWord newWords
  in Model word Nothing Set.empty Undecided

addGuess : String -> Model -> Model
addGuess guess model = 
  let g = String.toLower guess
      old = String.fromList (Set.toList model.guesses) 
      updated = if String.contains g old then old else old ++ g
      newGuesses = String.toList updated |> Set.fromList
  in Model model.word model.error newGuesses model.result |> calcResult

calcResult : Model -> Model
calcResult m = Model m.word m.error m.guesses (result m)
 
setError : Http.Error -> Model -> Model
setError err model = 
   let error = Just (toString err)
   in Model "" error Set.empty Undecided

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RefreshWord ->
      (model, RandomWords.getRandomWords NewWord)

    NewWord (Ok newWords) ->
      (refreshWord newWords model, Cmd.none)    

    NewWord (Err err) ->
      (setError err model, Cmd.none)

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

  
gameResult : Model -> String 
gameResult m = case m.result of 
                 Undecided -> ""
                 Lost      -> "\n YOU LOST!\n\n\n"
                 Won       -> "\n YOU WON!!\n\n\n"

view : Model -> Html Msg
view model =
  case (model.error, model.result) of
    (Nothing, Undecided) ->
      div [ style [] ]
        [ div [] [pre [] [text (board model)]]
          , div [] [pre [] [text (picture model)]]
          , div [] [pre [] [text ("Wrong guesses so far: " ++ (wrongSoFar model))]]
          , div [] [pre [] [text (gameResult model)]]
          , h2 [] [text (">>" ++ model.word)]
          , input [ placeholder "type your guesses here", onInput NewGuess, style [], value ""] []
          , button [ onClick RefreshWord ] [ text ">>Fetch A New Secret Word<<" ]
        ]

    (Nothing, _) ->
       div []
        [ div [] [pre [] [text (board model)]]
          , div [] [pre [] [text (picture model)]]
          , div [] [pre [] [text ("Wrong guesses: " ++ (wrongSoFar model))]]
          , div [] [pre [] [text (gameResult model)]]
          , h2 [] [text ("The word was: " ++ model.word)]
          , button [ onClick RefreshWord ] [ text ">>Fetch A New Secret Word<<" ]
        ]

    (Just errorMessage, _) ->
      div []
        [ h2 [] [text ("Error retrieving secret word: " ++ errorMessage)]
          , button [ onClick RefreshWord ] [ text ">>Try Again<<" ]
        ]  



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



