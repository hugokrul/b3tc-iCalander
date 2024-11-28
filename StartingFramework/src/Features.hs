module Features where

import DateTime
import Calendar
import qualified Data.Time as Cal
import Debug.Trace


-- Exercise 9
countEvents :: Calendar -> Int
countEvents (Calendar _ _ _ events _) = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents searchDate (Calendar _ _ _ events _) = findEvents' searchDate events

findEvents' :: DateTime -> [Event] -> [Event]
findEvents' searchDate [] = []
findEvents' searchDate@(DateTime (Date (Year searchYear) (Month searchMonth) (Day searchDay)) (Time (Hour searchHour) (Minute searchMinute) (Second searchSecond)) _) 
    (result@(Event _ _ _ 
        (DtStart (DateTime (Date (Year year1) (Month month1) (Day day1)) (Time (Hour hour1) (Minute minute1) (Second second1)) _)) 
        (DtEnd (DateTime (Date (Year year2) (Month month2) (Day day2)) (Time (Hour hour2) (Minute minute2) (Second second2)) _)) _ _ _ _):
        xs
    ) 
        = if differenceWithStart <= 0 && differenceWithFinish >= 0 then result : findEvents' searchDate xs else findEvents' searchDate xs
    where
        dayStart :: Cal.Day
        dayStart = Cal.fromGregorian (toInteger year1) month1 day1

        dayEnd :: Cal.Day
        dayEnd = Cal.fromGregorian (toInteger year2) month2 day2

        daySearchDate :: Cal.Day
        daySearchDate = Cal.fromGregorian (toInteger searchYear) searchMonth searchDay

        differenceWithStart :: Integer
        differenceWithStart = Cal.diffDays dayStart daySearchDate

        differenceWithFinish :: Integer
        differenceWithFinish = Cal.diffDays dayEnd daySearchDate




checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ _ _ events _) = checkOverlappingEvents events

checkOverlappingEvents :: [Event] -> Bool
checkOverlappingEvents [] = False
checkOverlappingEvents [_] = False
checkOverlappingEvents ((Event _ _ _ _ (DtEnd end1) _ _ _ _):(Event _ _ _ (DtStart start2) _ _ _ _ _):xs) = overlappingDates end1 start2

timeSpent :: String -> Calendar -> Int
timeSpent summary (Calendar _ _ _ events _) = timeSpentEvents summary events

timeSpentEvents :: String -> [Event] -> Int
timeSpentEvents _ [] = 0
timeSpentEvents summaryInput ((Event _ _ _ (DtStart start1) (DtEnd end1) _ (Just (Summary text)) _ _):xs) = countMinutes start1 end1
timeSpentEvents summaryInput ((Event _ _ _ _ _ _ Nothing _ _):xs) = timeSpentEvents summaryInput xs