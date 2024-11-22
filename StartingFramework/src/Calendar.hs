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

-- data CalProp = ProdId ProdId | Version Version
--     deriving (Eq, Ord, Show)

-- data ProdId = ProdId { text :: String }
--     deriving (Eq, Ord, Show)

-- data Version = Version { versionNumber :: Float}
--     deriving (Eq, Ord, Show)

data Event = Event 
    -- { runEvent :: EventProp }
    deriving (Eq, Ord, Show)

-- data EventProp = DtStamp DtStamp | Uid Uid | DtStart DtStart | DtEnd DtEnd | Description Description | Summary Summary | Location Location
--     deriving (Eq, Ord, Show)

-- data DtStamp = DtStamp { runDtStamp :: DateTime }
--     deriving (Eq, Ord, Show)

-- data Uid = Uid { text :: String }
--     deriving (Eq, Ord, Show)

-- data DtStart = DtStart { runDtStart :: DateTime }
--     deriving (Eq, Ord, Show)

-- data DtEnd = DtEnd { runDtEnd :: DateTime }
--     deriving (Eq, Ord, Show)

-- data Description = Description { text :: String }
--     deriving (Eq, Ord, Show)

-- data Summary = Summary { text :: String }
--     deriving (Eq, Ord, Show)

-- data Location = Location { text :: String}
--     deriving (Eq, Ord, Show)

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