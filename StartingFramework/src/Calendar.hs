module Calendar where

import ParseLib
import DateTime

-- We chose to make every item in the gramar its own data type
-- We would rather filter for non-correct calendars then to parse the callenders
-- Exercise 6
data Calendar = Calendar 
                        -- { runCalProp :: CalProp
                        --  , runEvent :: [Event] }
    deriving (Eq, Ord, Show)

data CalProp = Prodid ProdId | VersionNumber Version
    deriving (Eq, Ord, Show)

newtype ProdId = ProdId { runProdId :: String }
    deriving (Eq, Ord, Show)

newtype Version = Version { versionNumber :: Float}
    deriving (Eq, Ord, Show)

newtype Event = Event 
    { runEvent :: EventProp }
    deriving (Eq, Ord, Show)

data EventProp = Dtstamp DtStamp | EventUid Uid | Dtstart DtStart | Dtend DtEnd | EventDescription Description | EventSummary Summary | EventLocation Location
    deriving (Eq, Ord, Show)

newtype DtStamp = DtStamp { runDtStamp :: DateTime }
    deriving (Eq, Ord, Show)

newtype Uid = Uid { runUid :: String }
    deriving (Eq, Ord, Show)

newtype DtStart = DtStart { runDtStart :: DateTime }
    deriving (Eq, Ord, Show)

newtype DtEnd = DtEnd { runDtEnd :: DateTime }
    deriving (Eq, Ord, Show)

newtype Description = Description { runDescription :: String }
    deriving (Eq, Ord, Show)

newtype Summary = Summary { runSummary :: String }
    deriving (Eq, Ord, Show)

newtype Location = Location { runLocation :: String}
    deriving (Eq, Ord, Show)

-- Exercise 7
data Token = Token
    deriving (Eq, Ord, Show)

lexCalendar :: Parser Char [Token]
lexCalendar = undefined

parseCalendar :: Parser Token Calendar
parseCalendar = undefined

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined

-- Excercise 9
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: Calendar -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined