module Features where

import DateTime
import Calendar


-- Exercise 9
countEvents :: Calendar -> Int
countEvents (Calendar _ _ _ events _) = length events

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ _ _ events _) = checkOverlappingEvents events

checkOverlappingEvents :: [Event] -> Bool
checkOverlappingEvents [] = False
checkOverlappingEvents [_] = False
checkOverlappingEvents ((Event _ _ _ (DtStart start1) (DtEnd end1) _ _ _ _):(Event _ _ _ (DtStart start2) (DtEnd end2) _ _ _ _):xs) = overlappingDates (start1, end1) (start2, end2)

timeSpent :: String -> Calendar -> Int
timeSpent summary (Calendar _ _ _ events _) = timeSpentEvents summary events

timeSpentEvents :: String -> [Event] -> Int
timeSpentEvents _ [] = 0
timeSpentEvents summaryInput ((Event _ _ _ (DtStart start1) (DtEnd end1) _ summary _ _):xs) = countMinutes start1 end1