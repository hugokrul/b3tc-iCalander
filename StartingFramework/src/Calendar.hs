{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Calendar where

import ParseLib
import DateTime
import Debug.Trace
import Data.Maybe
import Data.List.Split
import Data.List
import Control.Monad
import Control.Applicative


-- We chose to make every item in the gramar its own data type
-- We would rather filter for non-correct calendars then to parse the calenders
-- Exercise 6
data Calendar = Calendar
                        { runCalendarBegin :: Begin
                        -- we used verprod to make sure the order in which the parser finds the version or prodid is not important
                        , runCalVersion :: VerProd
                        , runCalProdId :: VerProd
                        -- we checked if there was a minimum of 1 event in the parser using many1
                        , runCalEvent :: [Event]
                        , runCalendarEnd :: End }
    deriving (Eq, Ord)

instance Show Calendar where
    show = printCalendar

newtype Begin = Begin { runBegin :: String }
    deriving (Eq, Ord, Show)
newtype End = End { runEnd :: String }
    deriving (Eq, Ord, Show)

data VerProd = Prodid ProdId | Vers Version
    deriving (Eq, Ord, Show)

newtype ProdId = ProdId { runProdId :: String }
    deriving (Eq, Ord, Show)

newtype Version = Version { runVersionNumber :: String}
    deriving (Eq, Ord)

instance Show Version where show = const "VERSION:2.0"

data Event = Event  { runEventBegin :: Begin
                    , runEventDtStamp :: DtStamp
                    , runEventUid :: Uid
                    , runEventDtStart :: DtStart
                    , runEventDtEnd :: DtEnd
                    -- we used Maybe to indicate it is not required
                    , runEventDescription :: Maybe Description
                    , runEventSummary :: Maybe Summary
                    , runEventLocation :: Maybe Location
                    , runEventEnd :: End }
    deriving (Eq, Ord, Show)

-- the tokens for the events props
-- this makes sure we can parse all the props into a list, and then put it in the event constructors
data EventProp = DtStampProp DtStamp | UidProp Uid | DtStartProp DtStart | DtEndProp DtEnd
    | DescriptionProp (Maybe Description) | SummaryProp (Maybe Summary) | LocationProp (Maybe Location)
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

-- all the tokens that can be parsed
data StartTokens = BeginToken Begin | EndToken End | ProdIdToken ProdId | VersionToken Version | DtStampToken DtStamp | UidToken Uid
                                    | DtStartToken DtStart | DtEndToken DtEnd | DescriptionToken Description | SummaryToken Summary
                                    | LocationToken Location | No
    deriving (Eq, Ord, Show)

-- Exercise 7
newtype Token = Token { runToken :: StartTokens }
    deriving (Eq, Ord, Show)

-- consumes a line until it encounters an \n, then does the same to delete the \r
consumeLine :: Parser Char [Char]
consumeLine = greedy (satisfy (/= '\r')) <* greedy (satisfy (/= '\n'))

-- makes a list of all the lines to parse, splits them on the :, then combines them again if there is a space at the beginning
-- then it parses them to tokens
lexCalendar :: Parser Char [Token]
lexCalendar = do
    x <- listOf consumeLine (symbol '\n')
    let y = map (splitOn ":") x
    let result = checkToken (splitIntoTwo $ concatenateListsWithSpaces y)
    return result

-- merges the lists together if there is a space at the beginning
-- for example: [["Summary", "firstpart ", " second part"], ["description", "No space"]] -> [["Summary", "firstpart secondpart"], ["description", "No space"]]
splitIntoTwo :: [[String]] -> [[String]]
splitIntoTwo = map merge
    where
        merge :: [String] -> [String]
        merge input
            | length input > 2 = head input : [foldr (\x acc -> if null x then x ++ acc else if head x == ' ' then tail x++acc else x ++ acc) "" $ tail input]
            | otherwise = input

-- concatenates all the lists that start with a space at the beginning
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

-- parses the strings to tokens, skips it if it is not possible
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
        -- checks on if the date is an illegal date or not
        date input = if checkDateTime parsedDate then parsedDate else failDate
            where
                -- if the parseDateTime fails, it gives back an illegal date so the parser can fail
                parsedDate = fromMaybe failDate (run parseDateTime input)
-- if it fails, it skips the token.
checkToken (x:xs) = checkToken xs


parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> parseBegin "VCALENDAR" <*> parseVerProd <*> parseVerProd <*> parseEvents <*> parseEnd "VCALENDAR"

-- parses the begin token to a begin datatype, fails if it is not possible
parseBegin :: String -> Parser Token Begin
parseBegin typeOfBegin = do
    input <- anySymbol
    case input of
        Token (BeginToken (Begin text)) -> if text == typeOfBegin then return $ Begin text else failp
        _ -> failp

-- parses the version and prodId
-- they are in the same datatype so that the order in which they are found does not matter
parseVerProd :: Parser Token VerProd
parseVerProd = do
    input <- anySymbol
    case input of
        Token (VersionToken (Version string)) -> return $ Vers $ Version string
        Token (ProdIdToken (ProdId string)) -> return $ Prodid $ ProdId string
        _ -> failp

-- parses the end token to an end datatype
parseEnd :: String -> Parser Token End
parseEnd typeOfEnd = do
    input <- anySymbol
    case input of
        Token (EndToken (End text)) -> if text == typeOfEnd then return $ End text else failp
        _ -> failp

-- anySymbol totdat laatste endevent
-- endevent => beginevent moet ie doorgaan

-- checks if a token is of an end type
isEndType :: Token -> Bool
isEndType (Token (EndToken _)) = True
isEndType _ = False

-- parses one event
parseEvent :: Parser Token Event
parseEvent = parseBegin "VEVENT" *> parseProps <* parseEnd "VEVENT"


-- with many1 we make sure there is at least 1 prop in the list
-- sort makes sure it is the same order always. i.e., DtStamp, Uid, DtStart, DtEnd, Description, Summary, Location
parseProps :: Parser Token Event
parseProps = many1 (parseDtStamp
                <|> parseUid
                <|> parseDtStart
                <|> parseDtEnd
                <|> parseSummary
                <|> parseDescription
                -- this looks if eventProps is returned correctly, if it gives back Nothing one of the required eventProps is missing
                -- if eventPropsToEvent gives back nothing, it calls failp
                <|> parseLocation) >>= maybe failp return . eventPropsToEvent . sort

-- parses a dateStamp fails if it can't parse
parseDtStamp :: Parser Token EventProp
parseDtStamp = anySymbol >>= \input ->
    case input of
        -- failDate is called when run parseDateTime fails
        -- it should then fail the parser because one of the required dates is missing
        Token (DtStampToken (DtStamp date)) -> if date == failDate then failp else return $ DtStampProp (DtStamp date)
        _ -> failp

-- parses an uid fails if it can't parse
parseUid :: Parser Token EventProp
parseUid = anySymbol >>= \input ->
    case input of
        Token (UidToken (Uid text)) -> return $ UidProp (Uid text)
        _ -> failp

-- parses a dateStart fails if it can't parse
parseDtStart :: Parser Token EventProp
parseDtStart = anySymbol >>= \input ->
    case input of
        Token (DtStartToken (DtStart date)) -> if date == failDate then failp else return $ DtStartProp (DtStart date)
        _ -> failp

-- parses a dateEnd fails if it can't parse
parseDtEnd :: Parser Token EventProp
parseDtEnd = anySymbol >>= \input ->
    case input of
        Token (DtEndToken (DtEnd date)) -> if date == failDate then failp else return $ DtEndProp (DtEnd date)
        _ -> failp

-- parses a Summary fails if it can't parse
parseSummary :: Parser Token EventProp
parseSummary = anySymbol >>= \input ->
    case input of
        Token (SummaryToken (Summary text)) -> return $ SummaryProp (Just $ Summary text)
        _ -> failp

-- parses a Description fails if it can't parse
parseDescription :: Parser Token EventProp
parseDescription = anySymbol >>= \input ->
    case input of
        Token (DescriptionToken (Description text)) -> return $ DescriptionProp (Just $ Description text)
        _ -> failp

-- parses a Location fails if it can't parse
parseLocation :: Parser Token EventProp
parseLocation = anySymbol >>= \input ->
    case input of
        Token (LocationToken (Location text)) -> return $ LocationProp (Just $ Location text)
        _ -> failp

-- The order of the eventprop list is know, It has to contain a datestamp, uid, datestart and date end
-- The order of the rest of the list (description, summary, location) is also know, it uses the (!?) operator to test if it is there
eventPropsToEvent :: [EventProp] -> Maybe Event
eventPropsToEvent (DtStampProp dateStamp : UidProp uid : DtStartProp dateStart : DtEndProp dateEnd : rest) =
    Just $ Event (Begin "VEVENT") dateStamp uid dateStart dateEnd descr summ loc (End "VEVENT")
    where
        -- check if there exists a description in rest
        descr :: Maybe Description
        descr = case map Just (mapMaybe toDescription rest) of
            [] -> Nothing
            (x:_) -> x
        -- check if there exists a summary in rest
        summ :: Maybe Summary
        summ = case map Just (mapMaybe toSummary rest) of
            [] -> Nothing
            (x:_) -> x
        -- check if there exists a location in rest
        loc :: Maybe Location
        loc = case map Just (mapMaybe toLocation rest) of
            [] -> Nothing
            (x:_) -> x

        toSummary :: EventProp -> Maybe Summary
        toSummary (SummaryProp summary) = summary
        toSummary _ = Nothing
        toDescription :: EventProp -> Maybe Description
        toDescription (DescriptionProp description) = description
        toDescription _ = Nothing
        toLocation :: EventProp -> Maybe Location
        toLocation (LocationProp location) = location
        toLocation _ = Nothing
-- if one of the required EventProps is missing, the parser should fail
eventPropsToEvent _ = Nothing

-- parses at least one event
parseEvents :: Parser Token [Event]
parseEvents = many1 parseEvent

-- first lexes the calendar, then parses it
recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
-- pretty prints the calendar
printCalendar :: Calendar -> String
printCalendar (Calendar (Begin calBegin) verprod1 verprod2 events (End calEnd)) =
    "BEGIN:" ++ calBegin
    ++ "\r\n" ++
    case verprod1 of
        Vers (Version version) -> "VERSION:" ++ version
        Prodid (ProdId prodid) -> "PRODID:" ++ prodid
    ++ "\r\n" ++
    case verprod2 of
        Vers (Version version) -> "VERSION:" ++ version
        Prodid (ProdId prodid) -> "PRODID:" ++ prodid
    ++ "\r\n" ++
    printEvents events
    ++
    "END:" ++ calEnd

-- pretty prints all the events by recursivly calling printEvent with a newline
printEvents :: [Event] -> String
printEvents = foldr f ""
    where
        f x y = printEvent x ++ "\r\n" ++ y

-- pretty prints one event
printEvent :: Event -> String
printEvent (Event (Begin evBegin) (DtStamp dtstamp) (Uid uid) (DtStart dtstart) (DtEnd dtend) maybeDesc maybeSum maybeLoc (End evEnd)) =
    "BEGIN:" ++ evBegin
    ++ "\r\n" ++
    "DTSTAMP:" ++ show dtstamp
    ++ "\r\n" ++
    "UID:" ++ uid
    ++ "\r\n" ++
    "DTSTART:" ++ show dtstart
    ++ "\r\n" ++
    "DTEND:" ++ show dtend
    ++ "\r\n" ++
    (case maybeDesc of
        Just (Description desc) -> "DESCRIPTION:" ++ show desc ++ "\r\n"
        Nothing -> "") ++
    (case maybeSum of
        Just (Summary sum) -> "SUMMARY:" ++ show sum ++ "\r\n"
        Nothing -> "") ++
    (case maybeLoc of
        Just (Location loc) -> "LOCATION:" ++ show loc ++ "\r\n"
        Nothing -> "")
    ++ "END:" ++ evEnd