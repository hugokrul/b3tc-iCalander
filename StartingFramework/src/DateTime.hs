module DateTime where

import ParseLib

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
parseDateTime = parseDate <*> symbol 'T' <*> parseTime -- parse utc ????

parseDate :: Parser Char Date
parseDate = parseYear <*> parseMonth <*> parseDay

-- [0..3]
parseYear :: Parser Char Year
parseYear = parse <$> integer <*> statisfy (index /= 3)
-- [4..5]
parseMonth :: Parser Char Month
-- [6..7]
parserDay :: Parser Char Day
-- after T
parseTime :: Parser Char Time
parseTime = parseHour <*> parseMinute <*> parseSecond
-- [0..1]
parseHour :: Parser Char Hour
-- [2..3]
parseMinute :: Parser Char Minute
-- [4..5]
parseSecond :: Parser Char Second

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run = undefined

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime = undefined

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
