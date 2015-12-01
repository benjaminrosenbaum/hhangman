
import Data.List 
import Data.Functor
import Data.Char
import Control.Monad

-- for FetchWord
import Network.HTTP
import Data.List
import Data.Maybe
import Text.Regex

-- Drawing 

type Row = (String, Int)  -- a partial image on one row
type Stroke = [Row]       -- a partial picture, on various rows
type Picture = [Stroke]   -- a picture drawn by successive application of strokes

empty    = [("      ", r) | r <- [0..4]] 
crossbar = [(" ____ ", 0)]
pole     = [(" |    ", r) | r <- [1..4]]
base     = [("___   ", 5)]
noose    = [("    | ", 1)]
face     = [("    O ", 2)]
leftArm  = [("   -  ", 3)]
body     = [("    | ", 3)]
rightArm = [("     -", 3)]
leftLeg  = [("   /  ", 4)]
rightLeg = [("     L", 4)]

final = [empty, base, pole, crossbar, noose, face, body, leftArm, rightArm, leftLeg, rightLeg] --the complete hangman picture
maxMistakes = (length final) - 1

draw :: Picture -> String  --drawing a picture into a string representation
draw p =  unlines $ fst $ unzip $ foldl add [] (concat p)

step :: Int -> Picture
step n = take (n + 1) final -- the partial picture after n wrong guesses

add :: [Row] -> Row -> [Row]  --merging a row into a set of rows, overwriting earlier strokes with later ones
add [] r = [r]
add ((a,m):xs) (b,n) 
    | n == m    =  (zipWith overwrite a b, n):xs  --if the row number matches, overwrite its characters
    | otherwise = (a,m) : add xs (b,n)            --otherwise keep looking for which row to write it in

overwrite :: Char -> Char -> Char -- merging two characters
overwrite a ' ' = a -- spaces can't overwrite anything
overwrite _ a = a   -- otherwise, the second character overwrites the first

-- model 

type Guess = Char

data Result = Undecided | Lost | Won deriving (Eq, Ord, Enum)  
instance Show Result where
    show Undecided = ""
    show Lost = "\n YOU LOST!\n\n\n"
    show Won = "\n YOU WON!!\n\n\n"

data State = State { word :: String, guesses :: [Guess]} 
instance Show State where
     show s@(State w g) = let errs         = mistakes s 
                              drawing      = (draw $ step $ length errs)
                              wrongSoFar   = "\nWrong guesses so far: " ++ (intersperse ',' $ sort $ map toUpper errs) 
                              results       = show $ result s
                        in concat $ intersperse "\n" [drawing, (board s), wrongSoFar, results]  

mistakes :: State -> [Guess]
mistakes s = nub $ guesses s \\ word s

tooManyMistakes :: State -> Bool
tooManyMistakes = (maxMistakes <= ) . length . mistakes 

board :: State -> String
board s = intersperse ' ' $ map (\c -> if c `elem` guesses s then (toUpper c) else '_') $ word s

result :: State -> Result
result s@(State w gs) 
    | tooManyMistakes s  = Lost
    | all (`elem` gs) w  = Won
    | otherwise          =  Undecided

guess :: State -> Guess -> State
guess (State w gs) g = (State w $ g:gs)

finished :: State -> Bool
finished = (/= Undecided) . result

-- IO

clearScreen :: IO ()
clearScreen = putStrLn $ chr 27 : "c"

attempt :: IO State -> IO Guess -> IO State
attempt iostate inp = do s <- iostate
                         g <- inp
                         s' <- return $ guess s g
                         clearScreen
                         print s'
                         return s'

game :: [IO Guess] -> IO State 
game = foldl attempt initial  

--initial = return $ State "penguin" []
initial :: IO State
initial = do word <- fetchWord
             return $ State (fromMaybe "haskell" word) []

input :: IO Guess
input = do putStrLn "Please guess a letter:"
           toLower <$> getChar

loop :: IO State -> IO ()
loop s = do s' <- attempt s input
            if (finished s') 
              then when (result s' == Lost) $ putStrLn $ "\nThe word was: " ++ word s' ++ "\n"
              else loop (return s') 

main = do putStrLn "\n\nHey, let's play Hangman!\n\n"
          loop initial

-- FetchWord

fetchWord :: IO (Maybe String)
fetchWord = let url = getWordsUrl 10 
            in do json <- get url          
                  return $ extractWord json   -- TODO avoid capitalized words, words with spaces,   

get :: String -> IO String
get url = simpleHTTP (getRequest url) >>= getResponseBody

extractWord :: String -> Maybe String
extractWord s = let pattern = mkRegex "\"word\":\"([a-z]*)\""
                    result = matchRegex pattern s  
                in  result >>= (return . head)

getWordsUrl :: Int -> String
getWordsUrl n = let prefix = "http://api.wordnik.com/v4/words.json/randomWords?hasDictionaryDef=true&minCorpusCount=0"
                    range  = "&minLength=5&maxLength=15"
                    limit  = "&limit=" ++ (show n) 
                    key    = "&api_key=a2a73e7b926c924fad7001ca3111acd55af2ffabf50eb4ae5"
                in concat $ intersperse "&" [prefix, range, limit, key]
