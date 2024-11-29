module DateTime where

import ParseLib
import Control.Monad

import Debug.Trace
import Data.Char (isDigit)
import Text.Printf (printf)
import Data.Maybe (fromMaybe)
import Data.Time.Calendar.MonthDay (monthLength, MonthOfYear)
import qualified Data.Time.Calendar as Cal
import qualified Data.Time as Cal

-- class DateTimes a where
--     consumeParse :: Int -> (Int -> a) -> Parser Char a  
--     consumeParse i aa = consume i >>= \input -> 
--         case input of 
--             ls | checkAllDigits ls -> pure $ aa $ read input
--             _ -> empty

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc  :: Bool }
    deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

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

failDate :: DateTime
failDate = DateTime (Date (Year 0000) (Month 00) (Day 00)) (Time (Hour 00) (Minute 00) (Second 00)) True

checkAllDigits :: String -> Bool
checkAllDigits = all isDigit

consume :: Int -> Parser Char [Char]
consume n = replicateM n anySymbol

consumeParse :: Int -> (Int -> a) -> Parser Char a
consumeParse i parser = do
    let checkInput input = case input of
            ls | checkAllDigits ls -> pure $ parser $ read input
            _ -> empty
    consume i >>= checkInput

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> parseDate <* symbol 'T' <*> parseTime <*> parseUTC

parseUTC :: Parser Char Bool
parseUTC = consume 1 <<|> succeed [] >>= \ls -> if ls == "Z" then return True else return False

parseDate :: Parser Char Date
parseDate = Date <$> parseYear <*> parseMonth <*> parseDay

parseYear :: Parser Char Year
parseYear = consumeParse 4 Year

parseMonth :: Parser Char Month
parseMonth = consumeParse 2 Month

parseDay :: Parser Char Day
parseDay = consumeParse 2 Day

parseTime :: Parser Char Time
parseTime = Time <$> parseHour <*> parseMinute <*> parseSecond

parseHour :: Parser Char Hour
parseHour = consumeParse 2 Hour

parseMinute :: Parser Char Minute
parseMinute = consumeParse 2 Minute

parseSecond :: Parser Char Second
parseSecond = consumeParse 2 Second

-- Exercise 2
run :: (Show a, Show b) => Parser a b -> [a] -> Maybe b
-- parse :: Parser s a -> [s] -> [(a, [s])]
-- parse :: Parser String DateTime -> String -> [(DateTime, String)]
run parser input =
    case parse parser input of
        [] -> Nothing
        [(result, rest)] -> Just result
        (result, []):xs -> Just result
        (result, t):xs -> run parser t

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
checkDateTime (DateTime (Date (Year yearInt) (Month monthInt) (Day dayInt)) (Time (Hour hour) (Minute minute) (Second second)) utc) =
    if Cal.isLeapYear $ toInteger yearInt
        then
            dayInt <= monthLength True monthInt && monthInt > 0 && monthInt <= 12 && dayInt > 0 &&
            hour < 24 && minute < 60 && second < 60
        else
            dayInt <= monthLength False monthInt && monthInt > 0 && monthInt <= 12 && dayInt > 0 &&
            hour < 24 && minute < 60 && second < 60

-- Functions for the Calendar
overlappingDates :: DateTime -> DateTime -> Bool
overlappingDates 
    (DateTime (Date (Year year12) (Month month12) (Day day12)) (Time (Hour hour12) (Minute minute12) (Second second12)) utc12) 
    (DateTime (Date (Year year21) (Month month21) (Day day21)) (Time (Hour hour21) (Minute minute21) (Second second21)) utc21)
    | overlappingDays = True
    | sameDays = overlappingTime
    | otherwise = False
    where
        day1End :: Cal.Day
        day1End = Cal.fromGregorian (toInteger year12) month12 day12
        day2Start :: Cal.Day
        day2Start = Cal.fromGregorian (toInteger year21) month21 day21
        overlappingDays :: Bool
        overlappingDays = Cal.diffDays day2Start day1End < 0
        sameDays :: Bool
        sameDays = Cal.diffDays day1End day2Start == 0
        overlappingTime :: Bool
        overlappingTime = if hour12 == hour21 then (if minute12 == minute21 then second21 < second12 else minute21 < minute12) else hour21 < hour12

countMinutes :: DateTime -> DateTime -> Int
countMinutes (DateTime (Date (Year year11) (Month month11) (Day day11)) (Time (Hour hour1) (Minute minute1) (Second second1)) utc11)
    (DateTime (Date (Year year12) (Month month12) (Day day12)) (Time (Hour hour2) (Minute minute2) (Second second2)) utc12)  = minutes
    where
        day1 :: Cal.Day
        day1 = Cal.fromGregorian (toInteger year11) month11 day11

        day2 :: Cal.Day
        day2 = Cal.fromGregorian (toInteger year12) month12 day12

        differentDays :: Integer
        differentDays = Cal.diffDays day2 day1

        minutes :: Int
        minutes = 1440 * fromInteger differentDays + (hour2 - hour1) * 60 + (minute2 - minute1)