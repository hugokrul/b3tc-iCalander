module DateTime where

import ParseLib
import Control.Monad

import Debug.Trace
import Data.Char (isDigit)

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

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUTC

parseUTC :: Parser Char Bool
parseUTC = look >>= \input ->
    case drop 15 input of
        (x:xs) -> if x == 'Z' then pure True else pure False

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay
    
    -- Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = look >>= \input ->
    case take 4 input of
        ls | checkAllDigits ls -> pure $ Year $ read $ take 4 input
        _ -> empty  -- Fail for invalid input

checkAllDigits :: String -> Bool
checkAllDigits = all isDigit

-- [4..5]
parseMonth :: Parser Char Month
parseMonth = let takeMonth str = take 2 $ drop 4 str in
    look >>= \input ->
    case takeMonth input of
        ls | checkMonth ls -> pure $ Month $ read $ takeMonth input
        _ -> empty

checkMonth :: String -> Bool
checkMonth monthString = checkAllDigits monthString && smallerThenTwelve
    where 
        smallerThenTwelve = 12 >= read monthString

-- [6..7]   
parseDay :: Parser Char Day
parseDay = let takeDay str = take 2 $ drop 6 str in
    look >>= \input ->
    case takeDay input of
        ls | checkDay ls -> pure $ Day $ read $ takeDay input
        _ -> empty

checkDay :: String -> Bool
checkDay dayString = checkAllDigits dayString && checkMaximumDayCount
    where
        checkMaximumDayCount = dayString <= "31"


-- after T
parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

checkTime :: String -> Int -> Bool
checkTime hourString maximum = checkAllDigits hourString && checkMaximumDayCount
    where
        checkMaximumDayCount = hourString <= show maximum

parseHour :: Parser Char Hour
parseHour = let takeHour str = take 2 $ drop 9 str in
    look >>= \input ->
    case takeHour input of
        ls | checkTime ls 24 -> pure $ Hour $ read $ takeHour input
        _ -> empty


parseMinute :: Parser Char Minute
parseMinute = let takeMinute str = take 2 $ drop 11 str in
    look >>= \input ->
    case takeMinute input of
        ls | checkTime ls 60 -> pure $ Minute $ read $ takeMinute input
        _ -> empty
-- [4..5]

parseSecond :: Parser Char Second
parseSecond = let takeSecond str = take 2 $ drop 13 str in
    look >>= \input ->
    case takeSecond input of
        ls | checkTime ls 60 -> pure $ Second $ read $ takeSecond input
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
        "\nDate: " ++ show year ++ "/" ++ show month ++ "/" ++ show day ++ 
        "\nTime: " ++ show hour ++ ":" ++ show minute ++ ":" ++ show second ++
        "\nUTC?: " ++ show utc

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
