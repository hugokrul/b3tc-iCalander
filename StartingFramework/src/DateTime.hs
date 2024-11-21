module DateTime where

import ParseLib
import Control.Monad

import Debug.Trace

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
parseUTC = pure True

parseDate :: Parser Char Date
parseDate = do
    y <- parseYear
    m <- parseMonth
    d <- parseDay
    return (Date y m d)
    
    -- Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = look >>= \input ->
    trace ("test: " ++ input) $
    case input of
        (x:xs) | read [x] >= 0 && read [x] <= 9 -> pure $ Year $ read [x]
        _ -> failp  -- Fail for invalid input

--TODO: parse the first four digits of the input as the year.

-- [4..5]
parseMonth :: Parser Char Month
-- [6..7]   
parseMonth = pure (Month 11)

parseDay :: Parser Char Day
parseDay = pure (Day 21)
-- something like a parseInt to convert from int to day

-- after T
parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond
-- [0..1]
parseHour :: Parser Char Hour
parseHour = pure (Hour 18)
-- [2..3]

parseMinute :: Parser Char Minute
parseMinute = pure (Minute 19)
-- [4..5]

parseSecond :: Parser Char Second
parseSecond = pure (Second 00)

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
-- parse :: Parser s a -> [s] -> [(a, [s])]
-- parse :: Parser String DateTime -> String -> [(DateTime, String)]
run parser [] = Nothing
run parser input = case parse parser input of
    [(result, _)] -> Just result
    _ -> Nothing

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year year) (Month month) (Day day)) (Time (Hour hour) (Minute minute) (Second second)) utc)
    =
        show year ++ "/" ++ show month ++ "/" ++ show day ++ "\t" ++ show hour ++ ":" ++ show minute ++ ":" ++ show second

-- Exercise 4
parsePrint :: [Char] -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
