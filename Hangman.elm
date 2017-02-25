-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/http.html

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode 
import Regex



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
  , guesses : List Guess
  }

init : (Model, Cmd Msg)
init = (Model "" Nothing [], Cmd.none)


-- UPDATE


type Msg
  = RefreshWord
  | NewWord (Result Http.Error (List String))


isScrabblish : String -> Bool
isScrabblish contents = Regex.contains (Regex.regex "^[a-z]*$") contents 
  
firstScrabblish : (List String) -> Maybe String
firstScrabblish items = items |> List.filter isScrabblish |> List.head 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    RefreshWord ->
      (model, getRandomWords)

    NewWord (Ok newWords) ->
      let word = firstScrabblish newWords |> Maybe.withDefault ""
          guesses = []
      in (Model word Nothing guesses, Cmd.none)    

    NewWord (Err err) ->
      let error = Just (toString err)
          guesses = []
      in (Model "" error guesses, Cmd.none)


-- VIEW

board : Model -> String
--board s = intersperse ' ' $ map (\c -> if c `elem` guesses s then (toUpper c) else '_') $ word s
board m = m.word
          |> String.split ""
          |> String.join " " 


view : Model -> Html Msg
view model =
  case model.error of
    Nothing ->
      div []
        [ div [] [pre [] [text (board model)]]
          , h2 [] [text model.word]
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



-- HTTP


getWordsUrl : Int -> String
getWordsUrl n = let prefix = "http://api.wordnik.com/v4/words.json/randomWords?hasDictionaryDef=true&minCorpusCount=0"
                    range  = "minLength=5&maxLength=15"
                    limit  = "limit=" ++ (toString n) 
                    key    = "api_key=a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5"
                in String.concat ( List.intersperse "&" [prefix, range, limit, key] )


getRandomWords : Cmd Msg
getRandomWords =
  let
    url = getWordsUrl 10
  in
    Http.send NewWord (Http.get url decodeWords)


decodeWords : Decode.Decoder (List String)
decodeWords =
  Decode.list (Decode.at ["word"] Decode.string)
