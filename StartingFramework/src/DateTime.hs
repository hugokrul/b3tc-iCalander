module DateTime where

import ParseLib ( parse, Parser )

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { runYear  :: Int } deriving (Eq, Ord, Show)
newtype Month = Month { runMonth :: Int } deriving (Eq, Ord, Show)
newtype Day   = Day   { runDay   :: Int } deriving (Eq, Ord, Show)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour   { runHour   :: Int } deriving (Eq, Ord, Show)
newtype Minute = Minute { runMinute :: Int } deriving (Eq, Ord, Show)
newtype Second = Second { runSecond :: Int } deriving (Eq, Ord, Show)


-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <*> parseTime <*> parseUTC

parseUTC :: Parser Char Bool  
parseUTC = pure True

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = pure (Year 2024)

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
parseSecond = pure (Second 0)

-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
-- parse :: Parser s a -> [s] -> [(a, [s])]
-- parse :: Parser String DateTime -> String -> [(DateTime, String)]
run parser [] = Nothing
run parser input = Just $ fst $ head $ parse parser input

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date (Year year) (Month month) (Day day)) (Time (Hour hour) (Minute minute) (Second second)) utc) 
    = 
        show year ++ "/" ++ show month ++ "/" ++ show day ++ "\t" ++ show hour ++ ":" ++ show minute ++ ":" ++ show second

-- Exercise 4
parsePrint s = fmap printDateTime $ run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime = undefined
