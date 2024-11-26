module Calendar where

import ParseLib
import DateTime
import Debug.Trace


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

newtype Version = Version { runVersionNumber :: Float}
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

data StartTokens = BeginToken Begin | EndToken End | ProdIdToken ProdId | VersionToken Version | DtStampToken DtStamp | Uidtoken Uid
                                    | DtStartToken DtStart | DtEndToken DtEnd | DescriptionToken Description | SummaryToken Summary 
                                    | LocationToken Location
    deriving (Eq, Ord, Show)

-- Exercise 7
newtype Token = Token { runToken :: StartTokens }
    deriving (Eq, Ord, Show)

-- sequence $ replicate 4 anySymbol

consumeLine :: Parser Char [Char]
consumeLine = greedy (satisfy (/= '\r')) <* greedy (satisfy (/= '\n'))

consumeR :: Parser Char [Char]
consumeR = greedy (satisfy (/= '\r'))

lexCalendar :: Parser Char [Token]
lexCalendar = do
    x <- listOf consumeLine (symbol '\n') 
    return $ makeTokens x

makeTokens :: [[Char]] -> [Token]
makeTokens (x:xs) = do
    [Token (BeginToken (Begin "test"))]

    

parseCalendar :: Parser Token Calendar
parseCalendar = look >>= \input -> succeed (Calendar (Begin "VCALENDAR") (ProdId "-//hacksw/handcal//NONSGML v1.0//EN") (Version 2.0) [] (End "VCALENDAR"))

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= \input -> run parseCalendar input

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar = undefined