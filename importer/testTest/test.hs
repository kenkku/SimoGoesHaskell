import System.Environment
import Data.List
import Data.List.Split

import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Data.Time.Clock

import Data.Time.Calendar

main = do
        [s] <- getArgs
        fileContents <- readFile s
        print s
        
        let linesInFile = lines fileContents

        let days = parseDays $ filter (isPrefixOf "--- ") $ linesInFile
        --let days = filter (isPrefixOf "--- ") $ linesInFile
        --print days


        let messages = tail $ splitWhen (isPrefixOf "--- ") $ linesInFile 


        --print $ head messages

        let daysMessagesTuple = zip days messages

        --print $ daysMessagesTuple

        let penis = fst $ head daysMessagesTuple

        print penis

        return ()
        
parseDays days = map parseDay days 

parseDay day
    | isPrefixOf "--- Day" day = parseDayChanged day
    | isPrefixOf "--- Log opened" day = parseLogOpened day

parseDayChanged dayChanged =
    let splitDayChanged = splitPlaces [11,50] $ drop 16 dayChanged
        dayChangedUTC = (head splitDayChanged) ++ "00:00:00 UTC " ++ (last splitDayChanged)
    in parseTime defaultTimeLocale "%c" dayChangedUTC :: Maybe UTCTime

parseLogOpened logOpened = parseTime defaultTimeLocale "%c" (drop 15 logOpened) :: Maybe UTCTime

