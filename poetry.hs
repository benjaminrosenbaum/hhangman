import System.Random
import Test.QuickCheck

-- cf. http://developer.wordnik.com/docs.html#!/word/getTextPronunciations_get_5
-- cf. https://en.wikipedia.org/wiki/Arpabet
-- cf. https://www.wordsapi.com/docs#details


type StressPattern = [Char]
type Wrd = (String, StressPattern)

data PartOfSpeech = N1 | V1 | NS | VS | ADJ | ADV | ART | PREP | PRNOM1 | PRNOMS | PRACC1 | PRACCS | PRNS| CONJ
type Structure = [PartOfSpeech]

stressing :: StressPattern -> [String] -> [Wrd]
stressing pat ws = zip ws $ repeat pat

singleNouns :: [Wrd]
singleNouns = 
    stressing      "X" ["tree", "ball", "sun", "bark", "boat", "gold", "crate", "bear", "fox", "wolf", "deer", "hog", "house",
                        "witch", "wood", "wand", "pall", "coat", "smile", "bomb", "pine", "oak", "spruce", "light"]
    ++  stressing "OX" ["eclair", "duet", "Paulette", "surprise", "caress"] 
    ++ stressing  "XO" ["football", "rocket", "monad", "duel", "sparrow", "lion", "entry", "exit", "igloo", "silence", "freedom", "axe-blade", "city",
                        "poem", "poet", "mystic", "tower", "magic", "logic"]
    ++ stressing "XOX" ["nematode", "element" ]
    ++ stressing "OXO" ["computer", "electron"]


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
                --it is the east and juliet is the sun
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


pickIambicNoun = generate $ suchThat (elements singleNouns) (\w -> iambic snd w)



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