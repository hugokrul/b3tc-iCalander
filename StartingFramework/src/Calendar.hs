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
-- We would rather filter for non-correct calendars then to parse the callenders
-- Exercise 6
data Calendar = Calendar
                        { runCalendarBegin :: Begin
                        , runCalVerProd1 :: VerProd
                        , runCalVerProd2 :: VerProd
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
                    , runEventDescription :: Maybe Description
                    , runEventSummary :: Maybe Summary
                    , runEventLocation :: Maybe Location
                    , runEventEnd :: End }
    deriving (Eq, Ord, Show)

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

data StartTokens = BeginToken Begin | EndToken End | ProdIdToken ProdId | VersionToken Version | DtStampToken DtStamp | UidToken Uid
                                    | DtStartToken DtStart | DtEndToken DtEnd | DescriptionToken Description | SummaryToken Summary
                                    | LocationToken Location | No
    deriving (Eq, Ord, Show)

-- Exercise 7
newtype Token = Token { runToken :: StartTokens }
    deriving (Eq, Ord, Show)

consumeLine :: Parser Char [Char]
consumeLine = greedy (satisfy (/= '\r')) <* greedy (satisfy (/= '\n')) -- >>= \input -> return input

lexCalendar :: Parser Char [Token]
lexCalendar = do
    x <- listOf consumeLine (symbol '\n')
    let y = map (splitOn ":") x
    let result = checkToken (splitIntoTwo $ concatenateListsWithSpaces y)
    return result

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
parseCalendar = Calendar <$> parseBegin <*> parseVerProd <*> parseVerProd <*> parseEvents <*> parseEnd
-- testParse :: (a -> StartTokens) -> (String -> a) -> Parser Char a
-- testParse typeToken typee parser = do 
--     input <- anySymbol
--     case input of
--         Token (typeToken (typee text)) -> return $ typee text
--         _ -> failp

parseBegin :: Parser Token Begin
parseBegin = do
    input <- anySymbol
    case input of
        Token (BeginToken (Begin text)) -> return $ Begin text
        _ -> failp

parseVerProd :: Parser Token VerProd
parseVerProd = do
    input <- anySymbol
    case input of
        Token (VersionToken (Version string)) -> return $ Vers $ Version string
        Token (ProdIdToken (ProdId string)) -> return $ Prodid $ ProdId string
        _ -> failp

parseEnd :: Parser Token End
parseEnd = do
    input <- anySymbol
    case input of
        Token (EndToken (End text)) -> return $ End text
        _ -> failp

-- anySymbol totdat laatste endevent
-- endevent => beginevent moet ie doorgaan

isEndType :: Token -> Bool
isEndType (Token (EndToken _)) = True
isEndType _ = False


parseEvent :: Parser Token Event
parseEvent = parseBeginEvent *> parseProps <* parseEndEvent


parseProps :: Parser Token Event
-- with many1 we make sure there is at least 1 prop in the list
-- sort makes sure it is the same order always. i.e., DtStamp, Uid, DtStart, DtEnd, Description, Summary, Location
parseProps = eventPropsToEvent . sort <$> many1 (parseDtStamp
                                     <|> parseUid
                                     <|> parseDtStart
                                     <|> parseDtEnd
                                     <|> parseSummary
                                     <|> parseDescription
                                     <|> parseLocation)

parseDtStamp :: Parser Token EventProp
parseDtStamp = anySymbol >>= \input ->
    case input of
        Token (DtStampToken (DtStamp date)) -> return $ DtStampProp (DtStamp date)
        _ -> failp

parseUid :: Parser Token EventProp
parseUid = anySymbol >>= \input ->
    case input of
        Token (UidToken (Uid text)) -> return $ UidProp (Uid text)
        _ -> failp

parseDtStart :: Parser Token EventProp
parseDtStart = anySymbol >>= \input ->
    case input of
        Token (DtStartToken (DtStart date)) -> return $ DtStartProp (DtStart date)
        _ -> failp

parseDtEnd :: Parser Token EventProp
parseDtEnd = anySymbol >>= \input ->
    case input of
        Token (DtEndToken (DtEnd date)) -> return $ DtEndProp (DtEnd date)
        _ -> failp

parseSummary :: Parser Token EventProp
parseSummary = anySymbol >>= \input ->
    case input of
        Token (SummaryToken (Summary text)) -> return $ SummaryProp (Just $ Summary text)
        _ -> failp

parseDescription :: Parser Token EventProp
parseDescription = anySymbol >>= \input ->
    case input of
        Token (DescriptionToken (Description text)) -> return $ DescriptionProp (Just $ Description text)
        _ -> failp

parseLocation :: Parser Token EventProp
parseLocation = anySymbol >>= \input ->
    case input of
        Token (LocationToken (Location text)) -> return $ LocationProp (Just $ Location text)
        _ -> failp

(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < 0 = Nothing
    | otherwise = foldr (\x r k -> case k of 
                                    0 -> Just x
                                    _ -> r (k-1)) (const Nothing) xs n

eventPropsToEvent :: [EventProp] -> Event
eventPropsToEvent (DtStampProp dateStamp : UidProp uid : DtStartProp dateStart : DtEndProp dateEnd : rest) = 
    Event (Begin "VEVENT") dateStamp uid dateStart dateEnd descr summ loc (End "VEVENT")
    where
        descr :: Maybe Description
        descr = toDescription $ rest !? 0
        summ :: Maybe Summary
        summ = toSummary $ rest !? 0
        loc :: Maybe Location
        loc = toLocation $ rest !? 0

        toDescription :: Maybe EventProp -> Maybe Description
        toDescription Nothing = Nothing
        toDescription (Just (DescriptionProp description)) = description
        toDescription _ = Nothing

        toSummary :: Maybe EventProp -> Maybe Summary
        toSummary Nothing = Nothing
        toSummary (Just (SummaryProp summary)) = summary
        toSummary _ = Nothing

        toLocation :: Maybe EventProp -> Maybe Location
        toLocation Nothing = Nothing
        toLocation (Just (LocationProp location)) = location
        toLocation _ = Nothing

parseEvents :: Parser Token [Event]
parseEvents = many parseEvent >>= \input -> return input

parseEndEvent :: Parser Token End
parseEndEvent = do
    input <- anySymbol
    case input of
        Token (EndToken (End text)) -> if text == "VEVENT" then return $ End text else failp
        _ -> failp

parseBeginEvent :: Parser Token Begin
parseBeginEvent = do
    input <- anySymbol
    case input of
        Token (BeginToken (Begin text)) -> if text == "VEVENT" then return $ Begin text else failp
        _ -> failp

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run lexCalendar s >>= run parseCalendar

-- Exercise 8
printCalendar :: Calendar -> String
printCalendar (Calendar (Begin calBegin) verprod1 verprod2 events (End calEnd)) = 
    "BEGIN:" ++ calBegin
    ++ '\n':
    case verprod1 of
        Vers (Version version) -> "VERSION:" ++ show version
        Prodid (ProdId prodid) -> "PRODID:" ++ prodid
    ++ '\n':
    case verprod2 of
        Vers (Version version) -> "VERSION:" ++ show version
        Prodid (ProdId prodid) -> "PRODID:" ++ prodid
    ++ '\n':
    printEvents events
    ++ '\n':
    "END:" ++ calEnd

printEvents :: [Event] -> String
printEvents = foldr f ""
    where
        f x y = printEvent x ++ "\n" ++ y

printEvent :: Event -> String
printEvent (Event (Begin evBegin) (DtStamp dtstamp) (Uid uid) (DtStart dtstart) (DtEnd dtend) maybeDesc maybeSum maybeLoc (End evEnd)) =
    "\n" ++
    "BEGIN:" ++ show evBegin
    ++ "\n" ++
    "DTSTAMP:" ++ show dtstamp
    ++ '\n':
    "UID:" ++ show uid
    ++ '\n':
    "DTSTART:" ++ show dtstart
    ++ '\n':
    "DTEND:" ++ show dtend
    ++ "\n"++
    (case maybeDesc of
        Just (Description desc) -> "DESCRIPTION:" ++ show desc ++ "\n"
        Nothing -> "") ++
    (case maybeSum of
        Just (Summary sum) -> "SUMMARY:" ++ show sum ++ "\n"
        Nothing -> "") ++
    (case maybeLoc of
        Just (Location loc) -> "LOCATION:" ++ show loc ++ "\n"
        Nothing -> "")
    ++ "END:" ++ evEnd