
import Text.ParserCombinators.ReadP
import Control.Applicative

isVowel :: Char -> Bool
isVowel char = any (char ==) "aeiou"

vowel :: ReadP Char
vowel = satisfy isVowel

atLeastOneVowel :: ReadP [Char]
atLeastOneVowel = many1 vowel

airport :: ReadP String
airport = do
           code <- many1 (satisfy (\char -> char >= 'A' && char <= 'Z'))
           satisfy (== ' ')
           return code

digit :: ReadP Char
digit = satisfy (\char -> char >= '0' && char <= '9')

numbers :: Int -> ReadP Int
numbers digits = fmap read (count digits digit)

timestamp :: ReadP (Int, Int, Int)
timestamp = do
             day <- numbers 2
             hour <- numbers 2
             minute <- numbers 2
             string "Z "
             if day < 1 || day > 31 || hour > 23 || minute > 59 then
                pfail
             else
                return (day, hour, minute)

data WindInfo = WindInfo 
        { dir :: Int
        , speed :: Int
        , gusts :: Maybe Int
        }
        deriving Show

toMPS :: String -> Int -> Int
toMPS unit speed =
        case unit of
                "KT" -> div speed 2
                "MPS" -> speed

gustParser :: ReadP Int
gustParser = do
              satisfy (== 'G')
              numbers 2 <|> numbers 3

windInfo :: ReadP WindInfo
windInfo = do
            direction <- numbers 3
            speed <- numbers 2 <|> numbers 3
            gusts <- option Nothing (fmap Just gustParser)
            unit <- string "KT" <|> string "MPS"
            string " "
            return (WindInfo direction (toMPS unit speed) (fmap (toMPS unit) gusts))

data Report = Report
        { station :: String
        , time :: (Int, Int, Int)
        , wind :: WindInfo
        }
        deriving Show

metar :: ReadP Report
metar = do
         code <- airport
         time <- timestamp
         wind <- windInfo
         return (Report code time wind)
