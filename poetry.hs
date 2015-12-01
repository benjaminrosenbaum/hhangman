import System.Random
import Test.QuickCheck
import Data.List
import Control.Monad
import Data.Monoid

-- cf. http://developer.wordnik.com/docs.html#!/word/getTextPronunciations_get_5
-- cf. https://en.wikipedia.org/wiki/Arpabet
-- cf. https://www.wordsapi.com/docs#details

-- needed for random shuffle
import System.Random
import qualified Data.Map as Map

type StressPattern = [Char]
type StressedWrd = (String, StressPattern) 

data PartOfSpeech = N1 | V1 | NS | VS | ADJ | ADV | ART | PREP | PRNOM1 | PRNOMS | PRACC1 | PRACCS | PRNS| CONJ deriving (Eq, Ord, Enum, Show)
type Structure = [PartOfSpeech]

data WordData = WordData { word :: String, stresses :: StressPattern, partOfSpeech :: PartOfSpeech} deriving (Eq, Ord)
instance Show WordData where
     show w = word w

data SentenceModel = SentenceModel { stressPatterns :: StressPattern, partsOfSpeech :: Structure } deriving (Eq)     

type Vocabulary = [WordData]
type Sentence = [WordData]


iambic :: StressPattern -> Bool
iambic []       = True
iambic ('X':sp) = False
iambic ('O':sp) = trochaic sp

trochaic :: StressPattern -> Bool
trochaic []       = True
trochaic ('O':sp) = False
trochaic ('X':sp) = iambic sp

structures :: [Structure]
structures = [ 
                [ART, N1, V1],
                [ART, ADJ, ADJ, N1, V1, ADV],
                [NS, VS, NS, CONJ, NS, VS, NS],
                [NS, VS, ADJ, NS, CONJ, NS, VS, NS],
                [NS, VS, ADV, ADJ, NS, CONJ, NS, VS, NS],
                --it is the east and juliet is the sun
                [N1, V1, ART, N1, CONJ, N1, V1, ART, N1],
                --And they shall fetch thee jewels from the deep
                [CONJ, NS, VS, NS, NS, PREP, ART, N1],
                --we will perform in measure, time and place
                --be not her maid since she is envious
                --o that this too too solid flesh would melt
                --if music be the food of love, play on
                --can you come over here to eat tonight?
                --then your city ate one purple bomb
                [ADV, ADJ, N1, V1, ART, ADJ, N1]
                --batter my heart three-personed God
                --trampolining is very challenging
                -- cry 'havoc' and let slip the dogs of war
             ]

--TODO pickIambicNoun = generate $ suchThat (elements singularNouns) (\w -> iambic $ stresses w)

type Equator a = (a -> a -> Bool) -- a needs to be Eq a, but not sure how

modelOf :: Sentence -> SentenceModel
modelOf wds = let strs = concatMap stresses wds
                  struc = map partOfSpeech wds
              in (SentenceModel strs struc)

type ModelComparisonPredicate = (SentenceModel -> SentenceModel -> Bool)

{--compareModels :: ModelComparisonPredicateOperations -> SentenceModel -> SentenceModel -> Bool
compareModels ops s1 s2 = let ps1 = partsOfSpeech s1
                              ps2 = partsOfSpeech s2
                          in ((fst ops) (stressPatterns s1) (stressPatterns s2)) && (snd ops) ps2 ps2
  
--fullyMatches :: ModelComparisonPredicate
--fullyMatches = compareModels ((==), (==))
--}

partiallyMatches :: ModelComparisonPredicate
partiallyMatches actual canonical = ((stressPatterns actual) `isPrefixOf` (stressPatterns canonical)) && ((partsOfSpeech actual) `isPrefixOf` (partsOfSpeech canonical))

fullyMatches :: ModelComparisonPredicate
fullyMatches actual canonical = ((stressPatterns actual) == (stressPatterns canonical)) && ((partsOfSpeech actual) == (partsOfSpeech canonical))

 

conforms :: StressPattern -> Structure -> Sentence -> Bool
conforms pattern structure wds = let ss = concatMap stresses wds
                                     ps = map partOfSpeech wds
                                 in (ss `isPrefixOf` pattern) && (ps `isPrefixOf` structure)


fulfills :: StressPattern -> Structure -> Sentence -> Bool
fulfills pattern structure wds = let ss = concatMap stresses wds
                                     ps = map partOfSpeech wds
                                 in (ss == pattern) && (ps == structure)




isValidAddition :: StressPattern -> Structure -> Sentence -> WordData -> Bool
isValidAddition pattern structure soFar newWord = (not $ newWord `elem` soFar) && (conforms pattern structure $ soFar ++ [newWord]) 

type AdditionValidator = (Sentence -> WordData -> Bool)

--eitherIsValid :: AdditionValidator -> AdditionValidator -> AdditionValidator
--eitherIsValid a b = (\ws w -> (a ws w) || (b ws w))

-- ANDing of binary predicates
infixr 3 ^&&
(^&&) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
p ^&& q = \a b -> (p a b) && (q a b) 

-- ORing of binary predicates
infixr 2 ^||
(^||) :: (a -> b -> Bool) -> (a -> b -> Bool) -> a -> b -> Bool
p ^|| q = \a b -> (p a b) || (q a b) 


isValidForStructures :: StressPattern -> AdditionValidator
isValidForStructures pattern = let validators    = map (isValidAddition pattern) structures --TODO consider eliminating global reference "structures"
                               in foldl1 (^||) validators


--TODO does this already exist?
append :: [a] -> Maybe a -> Maybe [a]
append xs Nothing = Nothing
append xs (Just x) = Just (xs ++ [x])

lengthen :: AdditionValidator -> Vocabulary -> Sentence -> Maybe Sentence
lengthen isValid vocab soFar = soFar `append` (find (isValid soFar) vocab)

attemptAddition :: StressPattern -> Vocabulary -> Sentence -> Maybe Sentence
attemptAddition pattern vocab = lengthen (isValidForStructures pattern) vocab

--produce :: StressPattern -> Vocabulary -> Sentence -> [WordData] -> Maybe Sentence
--produce pattern vocab soFar rejects 
--  |  


-- making word lists

vocabulary :: Vocabulary
vocabulary = singularNouns ++ pluralNouns ++ articles ++ adjectives ++ adverbs ++ prepositions ++ conjunctions ++ singularVerbs ++ pluralVerbs

as :: PartOfSpeech -> [StressedWrd] -> [WordData]
as p sws = map (\sw -> WordData (fst sw) (snd sw) p) sws

stressing :: StressPattern -> [String] -> [StressedWrd]
stressing pat ws = zip ws $ repeat pat

--we can autopluralize words if the stress doesn't change
backwardsPluralize :: String -> String 
backwardsPluralize ('h':'c':cs)     = "sehc" ++ cs 
backwardsPluralize ('h':'s':cs)     = "sehs" ++ cs 
backwardsPluralize ('n':'a':'m':cs) =  "nem" ++ cs 
backwardsPluralize ('y':cs)         =  "sei" ++ cs 
backwardsPluralize ('s':cs)         =  "ses" ++ cs 
backwardsPluralize ('x':cs)         =  "sex" ++ cs 
backwardsPluralize ('f':cs)         =  "sev" ++ cs
backwardsPluralize cs = 's':cs

pluralize :: String -> String
pluralize w = reverse $ backwardsPluralize $ reverse w

pluralizeAll :: [StressedWrd] -> [StressedWrd]
pluralizeAll = map pl where pl (w, sp) = ((pluralize w), sp)

baseNouns :: [StressedWrd]
baseNouns = stressing        "X" ["tree", "ball", "sun", "bark", "boat", "gold", "crate", "bear", "hog", "wood", "wand", "food",
                                          "pall", "coat", "smile", "bomb", "pine", "oak", "spruce", "light", "deep", "love"]
            ++ stressing    "OX" ["eclair", "duet", "Paulette"] 
            ++ stressing    "XO" ["football", "rocket", "monad", "duel", "sparrow", "lion", "entry", "exit", "igloo", "silence", "freedom", "axe-blade", "city",
                                  "poem", "poet", "mystic", "tower", "magic", "logic", "music", "chickpea", "snowman", "novel", "wisdom", "jewel",
                                  "octave"]
            ++ stressing    "XOX" ["nematode", "element", "temperature", "cigarette", "magazine" ]
            ++ stressing    "XOO" ["novelist", "destiny", "government", "family" ]
            ++ stressing    "OXO" ["computer", "electron", "investment", "decision"]
            ++ stressing   "OXOX" ["community", "electrolyte" ]
            ++ stressing   "XOXO" ["information", "understanding", "television", "combination"]
            ++ stressing  "OXOXO" []
            ++ stressing  "XOXOX" []
            ++ stressing "OXOXOX" []
            ++ stressing "XOXOXO" ["prestidigitation"]

singularNouns :: [WordData]
singularNouns = as N1 $
    stressing        "X"  ["deer", "sheep", "fish", "fox", "house", "witch", "flesh"]
    ++ stressing    "OX"  ["surprise", "caress"] 
    ++ stressing    "XO"  []
    ++ stressing    "XOX" []
    ++ stressing    "XOO" []
    ++ stressing    "OXO" []
    ++ stressing   "OXOX" []
    ++ stressing   "XOXO" ["economics"]
    ++ stressing  "OXOXO" []
    ++ stressing  "XOXOX" []
    ++ stressing "OXOXOX" []
    ++ stressing "XOXOXO" []
    ++ baseNouns

pluralNouns :: [WordData]
pluralNouns = as NS $ 
    stressing        "X" ["deer", "sheep", "fish"]
    ++ stressing    "XO" ["foxes", "houses", "witches"]
    ++ stressing   "OXO" ["surprises", "caresses"]
    ++ pluralizeAll baseNouns

articles :: [WordData]
articles = as ART $ stressing "O" ["the", "your", "my", "their", "some", "one"] --TODO plural articles and a/an have their own rules

adjectives :: [WordData]
adjectives = as ADJ $ 
    stressing        "X"  ["bad", "good", "white", "black", "red", "gray", "damned", "dear", "mad", "old", "fresh", "sweet",
                           "numb", "broad", "peach", "plumb"]
    ++ stressing    "OX"  ["austere", "aligned", "opaque"] 
    ++ stressing    "XO"  ["foolish", "willing", "pompous", "golden", "orange", "purple", "simple", "oval", "novel", "higher"]
    ++ stressing    "OXO" ["elided", "attractive", "appalling", "transparent"]
    ++ stressing    "XOX" []
    ++ stressing    "XOO" ["ignorant"]
    ++ stressing   "OXOX" ["excitable", "intransigent"]

adverbs :: [WordData]
adverbs = as ADV $ 
    stressing        "X"  ["now", "then", "once", "still", "well"]
    ++ stressing    "OX"  ["enough"] 
    ++ stressing    "XO"  ["numbly", "gladly", "harshly", "boldly", "darkly", "brightly", "simply"]
    ++ stressing    "OXO" ["obtusely", "correctly", "abruptly", "austerely"]
    ++ stressing    "XOX" []
    ++ stressing    "XOO" ["terribly"]
    ++ stressing   "OXOX" ["excitably", "transparently"]


prepositions :: [WordData]
prepositions = as PREP $ 
    stressing        "O"  ["of", "on", "in", "from", "while", "by", "with", "when"]
    ++ stressing    "OX"  ["beyond", "beside", "beneath", "above"] 


conjunctions :: [WordData]
conjunctions = as PREP $ 
    stressing        "O"  ["and", "or", "but", "since"] 

baseVerbs :: [StressedWrd]    --consider tense, mood, transitivity
baseVerbs = stressing        "X" ["age", "fall", "move", "mourn", "eat", "climb", "glide", "crawl", "soar", "plot", "wave", "love",
                                          "lose", "greet", "smile", "bomb", "pine", "run", "give", "light", "cry"]
            ++ stressing    "OX" ["become", "allow", "entrance", "regard"] 
            ++ stressing    "XO" ["wonder", "wander", "sicken", "blacken", "coarsen" {- <= intransitive -} ]
            ++ stressing   "OXO" ["enlighten", "devour"]  
            ++ stressing  "OXOX" ["electrify", "abominate"]  
  
singularVerbs :: [WordData]
singularVerbs = as V1 $  pluralizeAll baseVerbs ++
              stressing    "OX" ["annoys"]

pluralVerbs :: [WordData]
pluralVerbs = as VS $ baseVerbs  ++
              stressing    "OX" ["annoy"]


{-
<English Sentence> =
       <Simple Sentence> |
       <Compound Sentence>

<Simple Sentence> =
       <Declarative Sentence> |
       <Interrogative Sentence> |
       <Imperative Sentence> |
       <Conditional Sentence>

<Compound Sentence> =
       <Simple Sentence> <conjunction> <Simple Sentence> |
       "Either" <Declarative Sentence> "or" <Declarative Sentence> |
       "Either" <Imperative Sentence> "or" <Imperative Sentence>   

       <Declarative Sentence> = <subject> <predicate>

<subject> = <simple subject> | 
   <compound subject>

<simple subject> = <noun phrase> | 
   <nominative personal pronoun>

<noun> = <noun> [<prep phr>*]

<adjective> = <adjective> ("and" | "or") <adjective>

<prep phr> = <preposition> <object>

<noun phrase> =
       "the" <specific proper noun> |
       <proper noun> |
       <non-personal pronoun> |
       <article> [<adverb>* <adjective>] <noun> |
       [<adverb>* <adjective>] <noun-plural> |
       <proper noun-possessive> [<adverb>* <adjective>] <noun> |
       <personal possessive adjective> [<adverb>* <adjective>] <noun> |
       <article> <common noun-possessive>
              [<adverb>* <adjective>] <noun> 

 <compound subject> =
       <simple subject> ("and" | "or") <simple subject> |
       "Either" <simple subject> "or" <simple subject> |
       "Neither" <simple subject> "nor" <simple subject> 

<predicate> = (<verb> | <verb phrase>) <complement>

<auxV> = "must" | "may" |"might" |
       "will" |"would" |"shall" |
       "should" |"can" |"could"

       <verb> = <V1s> |<V2s> |<V3s> |
       <V1p> |<V2p> |<V3p> |
       <Vpast> |<linking verb>

<linking verb> = "am" |"are" |"is" | "was"| "were" |
       "look" | "looks" | "looked" |
       "become" | "became" | "become" | ...

<verb phrase> =
       ("had" |"have" |"has") ["not"] <Vpastp> |
       ("had" |"have" |"has") ["not"] "been" [<Vpastp> | <Ving>] |
       <auxV> ["not"] "have" <Vpastp> |
       <auxV> ["not"] "have" "been" [<Vpastp> | <Ving>] |
       <auxV> ["not"] "be" [<Vpastp> | <Ving>] |
       <auxV> ["not"] <Vinf> |
       "ought" ("to" | "not") <Vinf> |
       "ought" ("to" | "not") "be" [<Vpastp> | <Ving>] |
       "ought" ("to" | "not") "have" <Vpastp> |
       "ought" ("to" | "not") "have" "been" [<Vpastp> | <Ving>] |
       ("do" |"does" |"did") ["not"] [<Vinf>] |
       ("am" |"are" |"is" |"was" |"were") ["not"] [<Vpastp> | <Ving>] |
       ("am" |"are" |"is" |"was" |"were") ["not"] "being" [<Vpastp>] |
       ("am" |"are" |"is" |"was" |"were") ["not"] "going" "to" [<Vinf>] 

<complement> =
       [[<indirect object>] <object>] |
       [<adverb>* <adjective>] |
       [<prep phr>*] |
       ["to" <Vinf> [<object>]] |
       [<Ving>] 

<indirect object> = <object> =
       <simple object> | <compound object> 

<simple object> = <noun phrase> |
       <objective personal pronoun>

<compound object> =
       <simple object> ("and" | "or") <simple object> 

<Interrogative Sentence> =      
   <Declarative Sentence>"?"
   "Who" <predicate>"?"  
   ("What" |"Which") [<adverb>* <adjective>] <noun> <predicate>"?"  
   ["What" |"When" |"Where" |"Who" |"To whom" |"Why"] ("do" |"does" |"don't" |"doesn't" |"did" |"didn't") <subject> <predicate>"?"  
   "Which"  [<noun phrase>]("do" |"does" |"don't" |"doesn't" |"did" |"didn't") <subject> <predicate>"?"  
   ["What" |"Which" |"When" |"Where" |"Who" |"To whom" | "Why"] ("has" |"have" |"hasn't" |"haven't") <subject> <predicate>"?"  
   ["What" |"Which" |"When" |"Where" |"Who" |"To whom" | "Why"] ("are" |"is" |"was" |"were" | "aren't" |"isn't" |"wasn't" |"weren't") <subject> [<adverb>* <adjective> | <prep phr>* | <predicate>]"?" 

<Imperative Sentence> = <predicate> = <verb>  <complement>

<Dependent Clause> = ("if" | "when") <Declarative Sentence>

<Independent Clause> = <Declarative Sentence> | <Interrogative Sentence>

<Conditional Sentence> =
    <Dependent Clause>"," <Independent Clause> |
    <Independent Clause> <Dependent Clause> 

-}


--TESTS

pattern = "OXOXOX"
structure = [NS, VS, NS, CONJ, NS, VS, NS]

eclairs = WordData "eclairs" "OX" NS
provide = WordData "provide" "OX" VS
frag = [eclairs, provide]
abhor = WordData "abhor" "OX" VS

iambPhrase = isValidForStructures "OXOXOXOX" 
dactylPhrase = isValidForStructures "XOOXOOXOO"

moreIambs = attemptAddition "OXOXOXOXOX" 
bestIambs v = last $ takeWhile (\n -> n /= Nothing) $ iterate (>>= moreIambs v) (Just [])

pLineWords n = bestIambs $ shuffle n vocabulary
pLine n = ( (pLineWords n) >>= Just . unwords .  Data.List.map word)
poem lines = [ pLine n | n <- [1..lines]]
readPoem lines = putStrLn $ unwords $ intersperse "\n" $ map (\(Just v) -> v) $ poem lines


-- random shuffle, from https://wiki.haskell.org/Random_shuffle, names changed, here because I haven't gotten the hang of importing haskell packages. 


 
fisherYatesStep :: RandomGen g => (Map.Map Int a, g) -> (Int, a) -> (Map.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((Map.insert j x . Map.insert i (m Map.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (Map.elems x, y)
    numerate = zip [1..]
    initial x gen = (Map.singleton 0 x, gen)

shuffle :: Int -> [a] -> [a]
shuffle i lst = fst $ fisherYates (mkStdGen i) lst








