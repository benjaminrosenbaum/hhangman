module Drawing exposing(..)


type alias Row = (String, Int)  -- a partial image on one row
type alias Stroke = List Row       -- a partial picture, on various rows
type alias Picture = List Stroke   -- a picture drawn by successive application of strokes

empty    =   "      " |> acrossRows 0 4
crossbar = [(" ____ ", 0)]
pole     =   " |    " |> acrossRows 1 4
base     = [("___   ", 5)]
noose    = [("    | ", 1)]
face     = [("    O ", 2)]
leftArm  = [("   -  ", 3)]
body     = [("    | ", 3)]
rightArm = [("     -", 3)]
leftLeg  = [("   /  ", 4)]
rightLeg = [("     L", 4)]

acrossRows : Int -> Int -> String -> List ( String, Int )
acrossRows top bottom pattern = List.map (\r -> (pattern, r)) (List.range top bottom)

--the complete hangman picture
final = [empty, base, pole, crossbar, noose, face, body, leftArm, rightArm, leftLeg, rightLeg] 
maxMistakes = (List.length final) - 1

-- the partial picture after n wrong guesses
step : Int -> Picture
step n = List.take (n + 1) final 

-- merge two rows, overwriting non-space characters 
merge : String -> String -> String
merge a b = List.map2 (\a b -> if b == ' ' then a else b) (String.toList a) (String.toList b) |> String.fromList

-- merge a row into a set of rows, overwriting earlier strokes with later ones
add : Row -> List Row -> List Row  
add r rs =
    case (r, rs) of
        (r, []) -> [r]                                -- trivial case, add a row to an empty list
        ((b,n), (a,m)::rs) ->                         -- try adding (b,n) to a list whose first element is (a,m)
            if n == m then (merge a b, n)::rs         -- row numbers match, overwrite the characters
            else (a,m) :: add (b,n) rs                -- otherwise keep looking recursively for the row to overwrite

-- draw the picture
draw : Picture -> String  --drawing a picture into a string representation
draw p = List.concat p
         |> List.foldl add [] 
         |> List.unzip 
         |> Tuple.first 
         |> String.join "\n"

-- draw the picture at step N
drawAtStep : Int -> String
drawAtStep n = draw (step n)


