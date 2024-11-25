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
        ls | checkAllDigits ls -> pure $ Month $ read $ takeMonth input
        _ -> empty

-- parseDay looks only at the digits on index [6..7] and converts that to Day
parseDay :: Parser Char Day
parseDay = let takeDay str = take 2 $ drop 6 str in
    look >>= \input ->
    case takeDay input of
        ls | checkAllDigits ls -> pure $ Day $ read $ takeDay input
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
        ls | checkAllDigits ls -> pure $ Hour $ read $ takeHour input
        _ -> empty

-- looks at the second 2 digits after T ([11.12]) and converts that to Minute
parseMinute :: Parser Char Minute
parseMinute = let takeMinute str = take 2 $ drop 11 str in
    look >>= \input ->
    case takeMinute input of
        ls | checkAllDigits ls -> pure $ Minute $ read $ takeMinute input
        _ -> empty

-- looks at the third 2 digits after T ([13.14]) and converts that to Second
parseSecond :: Parser Char Second
parseSecond = let takeSecond str = take 2 $ drop 13 str in
    look >>= \input ->
    case takeSecond input of
        ls | checkAllDigits ls -> pure $ Second $ read $ takeSecond input
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
checkDateTime (DateTime (Date (Year yearInt) (Month monthInt) (Day dayInt)) (Time (Hour hour) (Minute minute) (Second second)) utc) =
    if Cal.isLeapYear $ toInteger yearInt
        then
            dayInt <= monthLength True monthInt && monthInt > 0 && monthInt <= 12 && dayInt > 0 &&
            hour < 24 && minute < 60 && second < 60
        else
            dayInt <= monthLength False monthInt && monthInt > 0 && monthInt <= 12 && dayInt > 0 &&
            hour < 24 && minute < 60 && second < 60

-- Functions for the Calendar
overlappingDates :: (DateTime, DateTime) -> (DateTime, DateTime) -> Bool
overlappingDates 
    (   DateTime (Date (Year year11) (Month month11) (Day day11)) (Time (Hour hour11) (Minute minute11) (Second second11)) utc11, 
        DateTime (Date (Year year12) (Month month12) (Day day12)) (Time (Hour hour12) (Minute minute12) (Second second12)) utc12) 

    (   DateTime (Date (Year year21) (Month month21) (Day day21)) (Time (Hour hour21) (Minute minute21) (Second second21)) utc21, 
        DateTime (Date (Year year22) (Month month22) (Day day22)) (Time (Hour hour22) (Minute minute22) (Second second22)) utc22)
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