module Calendar where

import ParseLib
import DateTime
import Debug.Trace

import Data.Maybe
import Data.List.Split


-- We chose to make every item in the gramar its own data type
-- We would rather filter for non-correct calendars then to parse the callenders
-- Exercise 6
data Calendar = Calendar 
                        { runCalendarBegin :: Begin
                        , runCalProdId :: ProdId
                        , runCalVersion :: Version
                        , runCalEvent :: [Event]
                        , runCalendarEnd :: End }
    deriving (Eq, Ord, Show)

newtype Begin = Begin { runBegin :: String }
    deriving (Eq, Ord, Show)
newtype End = End { runEnd :: String }
    deriving (Eq, Ord, Show)

newtype ProdId = ProdId { runProdId :: String }
    deriving (Eq, Ord, Show)

newtype Version = Version { runVersionNumber :: String}
    deriving (Eq, Ord, Show)

data Event = Event  { runEventBegin :: Begin
                    , runEventDtStamp :: DtStamp
                    , runEventUid :: Uid
                    , runEventDtStart :: DtStart
                    , runEventDtEnd :: DtEnd
                    , runEventDescription :: Maybe Description
                    , runEventSummary :: Maybe Summary
                    , runEventLocation :: Maybe Location
                    , runEventEnd :: End }
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

data StartTokens = BeginToken Begin | EndToken End | ProdIdToken ProdId | VersionToken Version | DtStampToken DtStamp | UidToken Uid
                                    | DtStartToken DtStart | DtEndToken DtEnd | DescriptionToken Description | SummaryToken Summary 
                                    | LocationToken Location
    deriving (Eq, Ord, Show)

-- Exercise 7
newtype Token = Token { runToken :: StartTokens }
    deriving (Eq, Ord, Show)

consumeLine :: Parser Char [Char]
consumeLine = greedy (satisfy (/= '\r')) <* greedy (satisfy (/= '\n'))

consumeColon :: Parser Char [Char]
consumeColon = greedy (satisfy (/= ':')) >>= \input -> return input

lexCalendar :: Parser Char [Token]
lexCalendar = do
    x <- listOf consumeLine (symbol '\n')
    let y = map (splitOn ":") x
    let result = checkToken (splitIntoTwo $ concatenateListsWithSpaces y)
    trace (show result) $ return result

splitIntoTwo :: [[String]] -> [[String]]
splitIntoTwo = map merge

merge :: [String] -> [String]
merge input
    | length input > 2 = head input : [foldr (\x acc -> if head x == ' ' then tail x++acc else x ++ acc) "" $ tail input]
    | otherwise = input

concatenateListsWithSpaces :: [[String]] -> [[String]]
concatenateListsWithSpaces [] = []
concatenateListsWithSpaces (x:xs) = go x xs
  where
    go acc [] = [acc] -- If no more elements, return the accumulated list
    go acc (y:ys)
      | startsWithSpace (head y) = go (acc ++ y) ys -- Concatenate if starts with a space
      | otherwise = acc : go y ys -- Add accumulated to result and start new

    -- Helper function to check if a string starts with a space
    startsWithSpace (c:_) = c == ' '
    startsWithSpace _ = False

-- TODO: check if token has a space and only one character, then it needs to be concatenated with the token in front
checkToken :: [[String]] -> [Token]
checkToken [] = []
checkToken [[]] = []
checkToken ([token, text]:xs) = case token of
    "BEGIN"         -> Token (BeginToken (Begin text))              : checkToken xs
    "END"           -> Token (EndToken (End text))                  : checkToken xs
    "PRODID"        -> Token (ProdIdToken (ProdId text))            : checkToken xs
    "VERSION"       -> Token (VersionToken (Version text))          : checkToken xs
    "DTSTAMP"       -> Token (DtStampToken (DtStamp (date text)))   : checkToken xs
    "UID"           -> Token (UidToken (Uid text))                  : checkToken xs
    "DTSTART"       -> Token (DtStartToken (DtStart (date text)))   : checkToken xs
    "DTEND"         -> Token (DtEndToken (DtEnd (date text)))       : checkToken xs
    "DESCRIPTION"   -> Token (DescriptionToken (Description text))  : checkToken xs
    "SUMMARY"       -> Token (SummaryToken (Summary text))          : checkToken xs
    "LOCATION"      -> Token (LocationToken (Location text))        : checkToken xs
    _               -> checkToken xs
    where
        date :: String -> DateTime
        date input = fromJust (run parseDateTime input)
checkToken (x:xs) = checkToken xs

parseCalendar :: Parser Token Calendar
-- placeholder, this should put the tokens in the calendar datastructure
parseCalendar = look >>= \input -> succeed (Calendar (Begin "VCALENDAR") (ProdId "-//hacksw/handcal//NONSGML v1.0//EN") (Version "2.0") [] (End "VCALENDAR"))

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined