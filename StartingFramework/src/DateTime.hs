module DateTime where

import ParseLib
import Control.Monad

import Debug.Trace
import Data.Char (isDigit)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord)

checkAllDigits :: String -> Bool
checkAllDigits = all isDigit

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUTC

parseUTC :: Parser Char Bool
parseUTC = look >>= \input ->
    -- looks at all the last symbol
    -- if that is a Z it is in UTC
    -- if no last symbol is given, it is not UTC
    case drop 15 input of
        (x:xs) -> if x == 'Z' then pure True else pure False

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = look >>= \input ->
    -- Looks at the first 4 digits and converts that to year
    case take 4 input of
        ls | checkAllDigits ls -> pure $ Year $ read $ take 4 input
        _ -> empty  -- Fail for invalid input

-- parseMonth looks only at the digits on index [4..5] and converts that to Month
parseMonth :: Parser Char Month
parseMonth = let takeMonth str = take 2 $ drop 4 str in
    look >>= \input ->
    case takeMonth input of
        ls | checkMaximum ls 12 && lsInt >= 1 -> pure $ Month $ read $ takeMonth input
            where
                lsInt :: Int
                lsInt = read ls
        _ -> empty

-- parseDay looks only at the digits on index [6..7] and converts that to Day
parseDay :: Parser Char Day
parseDay = let takeDay str = take 2 $ drop 6 str in
    look >>= \input ->
    case takeDay input of
        ls | checkMaximum ls 31 && lsInt >= 1 -> pure $ Day $ read $ takeDay input
            where
                lsInt ::Int
                lsInt = read ls
        _ -> empty

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

-- checks if the number is bigger then a maximum number given
checkMaximum :: String -> Int -> Bool
checkMaximum input maximum = checkAllDigits input && checkMaximumDayCount
    where
        checkMaximumDayCount = input <= show maximum

-- looks at the first 2 digits after T ([9..10]) and converts that to Hour
parseHour :: Parser Char Hour
parseHour = let takeHour str = take 2 $ drop 9 str in
    look >>= \input ->
    case takeHour input of
        ls | checkMaximum ls 24 -> pure $ Hour $ read $ takeHour input
        _ -> empty

-- looks at the second 2 digits after T ([11.12]) and converts that to Minute
parseMinute :: Parser Char Minute
parseMinute = let takeMinute str = take 2 $ drop 11 str in
    look >>= \input ->
    case takeMinute input of
        ls | checkMaximum ls 60 -> pure $ Minute $ read $ takeMinute input
        _ -> empty

-- looks at the third 2 digits after T ([13.14]) and converts that to Second
parseSecond :: Parser Char Second
parseSecond = let takeSecond str = take 2 $ drop 13 str in
    look >>= \input ->
    case takeSecond input of
        ls | checkMaximum ls 60 -> pure $ Second $ read $ takeSecond input
        _ -> empty

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
-- parse :: Parser s a -> [s] -> [(a, [s])]
-- parse :: Parser String DateTime -> String -> [(DateTime, String)]
run parser [] = Nothing
run parser input = case parse parser input of
    [(result, empty)] -> Just result
    _ -> Nothing

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year year) (Month month) (Day day)) (Time (Hour hour) (Minute minute) (Second second)) utc)
    =
        (\x -> replicate (4-length x) '0' ++ x) (show year) ++
        (\x -> replicate (2-length x) '0' ++ x) (show month) ++
        (\x -> replicate (2-length x) '0' ++ x) (show day) ++ 
        "T" ++ 
        (\x -> replicate (2-length x) '0' ++ x) (show hour) ++
        (\x -> replicate (2-length x) '0' ++ x) (show minute) ++
        (\x -> replicate (2-length x) '0' ++ x) (show second) ++ 
        if utc then "Z" else ""

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
