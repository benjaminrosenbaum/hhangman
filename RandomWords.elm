module RandomWords exposing(getRandomWords, selectWord)

--exposing(getRandomWords, selectWord) 

import Http
import Json.Decode as Decode 
import Regex




getRandomWords : (Result Http.Error (List String) -> a) -> Cmd a
getRandomWords cmd =
  let
    url = getWordsUrl 10
  in
    Http.send cmd (Http.get url decodeWords)


selectWord : (List String) -> String
selectWord newWords = firstScrabblish newWords |> Maybe.withDefault ""


-- PRIVATE

--TODO: difficulty levels as corpus count log: 0 = hard, 1000 = middle, 100000 = easy 
-- or we could do it based on http://api.wordnik.com:80/v4/word.json/the/frequency?useCanonical=false&startYear=1800&endYear=2012&api_key=a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5
-- and extract totalCount from the return {}. At present, {  "totalCount": 4414284 }


getWordsUrl : Int -> String
getWordsUrl n = let prefix = "http://api.wordnik.com/v4/words.json/randomWords?hasDictionaryDef=true&minCorpusCount=0"
                    range  = "minLength=5&maxLength=15"
                    limit  = "limit=" ++ (toString n) 
                    key    = "api_key=a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5"
                in String.concat ( List.intersperse "&" [prefix, range, limit, key] )



decodeWords : Decode.Decoder (List String)
decodeWords =
  Decode.list (Decode.at ["word"] Decode.string)



isScrabblish : String -> Bool
isScrabblish contents = Regex.contains (Regex.regex "^[a-z]*$") contents 
  
firstScrabblish : (List String) -> Maybe String
firstScrabblish items = items |> List.filter isScrabblish |> List.head 



