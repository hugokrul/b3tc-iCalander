-- You can use this file to test your functions: `cabal run` executes main.
-- For example, if main is set to mainDateTime or mainCalendar:
-- echo "19970610T172345Z" | cabal run
-- cat examples/bastille.ics | cabal run
-- Feel free to use ghci instead, or to change functions here to test whatever you want.
-- We'll ignore anything in this file when grading!

-- This code is written by Hugo Krul en Tijmen Vis


module Main where
import DateTime
import Calendar
import Features
import System.Environment
import System.IO


data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

testDate1Start :: DateTime
testDate1Start = DateTime (Date (Year 2023) (Month 2) (Day 20)) (Time (Hour 00) (Minute 00) (Second 00)) True
testDate1Finish :: DateTime
testDate1Finish = DateTime (Date (Year 2023) (Month 2) (Day 21)) (Time (Hour 00) (Minute 01) (Second 00)) True

testDate2Start :: DateTime
testDate2Start = DateTime (Date (Year 2023) (Month 2) (Day 21)) (Time (Hour 00) (Minute 00) (Second 00)) True
testDate2Finish :: DateTime
testDate2Finish = DateTime (Date (Year 2023) (Month 2) (Day 26)) (Time (Hour 00) (Minute 00) (Second 00)) True

testDate3Start :: DateTime
testDate3Start = DateTime (Date (Year 2023) (Month 2) (Day 22)) (Time (Hour 00) (Minute 0) (Second 00)) True
testDate3Finish :: DateTime
testDate3Finish = DateTime (Date (Year 2023) (Month 2) (Day 25)) (Time (Hour 00) (Minute 00) (Second 00)) True

testEvent1 :: Event
testEvent1 = Event (Begin "VEVENT") (DtStamp testDate1Start) (Uid "12345@example.com") (DtStart testDate1Start) (DtEnd testDate1Finish) Nothing (Just (Summary "test")) Nothing (End "VEVENT")

testEvent2 :: Event
testEvent2 = Event (Begin "VEVENT") (DtStamp testDate1Start) (Uid "12345@example.com") (DtStart testDate2Start) (DtEnd testDate2Finish) Nothing Nothing Nothing (End "VEVENT")

testEventList :: [Event]
testEventList = [testEvent2, testEvent1]

testCalendar :: Calendar
testCalendar = Calendar (Begin "VCALENDAR") (Prodid (ProdId "-//hacksw/handcal//NONSGML v1.0//EN")) (Vers (Version "2.0")) testEventList (End "VCALENDAR")

-- data Calendar = Calendar 
--                         { runCalendarBegin :: Begin
--                         , runCalVerProd1 :: VerProd
--                         , runCalVerProd2 :: VerProd
--                         , runCalEvent :: [Event]
--                         , runCalendarEnd :: End }

main :: IO ()
main = do
  setNewlineTranslations
  mainCalendar

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
  where
    processInput input = map (run parseDateTime) (lines input)
    processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
    printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = interact (show . recognizeCalendar)

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar path = do
  string <- readFileWindows path
  return $ recognizeCalendar string

-- These three functions fight Windows newline translations:
-- without them, on Windows machines, "\r\n" will be read as "\n"
-- and "\n" will be written as "\r\n".
-- Test using these functions rather than "readFile", and parse  newlines as "\r\n",
-- to make sure your parser works on all operating systems (i.e. also for your grader)!
setNewlineTranslations :: IO ()
setNewlineTranslations = do
  hSetNewlineMode stdin  noNewlineTranslation
  hSetNewlineMode stdout noNewlineTranslation
readFileWindows :: FilePath -> IO String
readFileWindows p = withBinaryFile p ReadMode hGetContents
writeFileWindows :: FilePath -> String -> IO ()
writeFileWindows p s = withBinaryFile p WriteMode (`hPutStr` s)
