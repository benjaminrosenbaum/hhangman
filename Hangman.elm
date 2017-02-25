
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


type alias Model = Maybe String

init : (Model, Cmd Msg)
init = (Nothing, Cmd.none)


-- UPDATE


type Msg
  = MorePlease
  | NewWord (Result Http.Error (List String))


isScrabblish : String -> Bool
isScrabblish contents = Regex.contains (Regex.regex "^[a-z]*$") contents 
  
firstScrabblish : (List String) -> Maybe String
firstScrabblish items = items |> List.filter isScrabblish |> List.head 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    MorePlease ->
      (model, getRandomWords)

    NewWord (Ok newWords) ->
      (firstScrabblish newWords, Cmd.none) 

    NewWord (Err err) ->
      (Just (toString err), Cmd.none)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [text (Maybe.withDefault "--" model)]
    , button [ onClick MorePlease ] [ text ">>Fetch<<" ]
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
