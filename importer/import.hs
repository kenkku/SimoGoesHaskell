import System.Environment
import Data.List
import Data.List.Split

import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Data.Time.Clock

import Data.Time.Calendar

import Data.Time

import Network.Curl

main = do
        [s] <- getArgs
        fileContents <- readFile s
        print s
        
        let linesInFile = lines fileContents

        let days = parseDays $ filter (isDayChange) $ linesInFile
        --let days = filter (isPrefixOf "--- ") $ linesInFile
        --print days

        --Strips out everything except day changeds and 
        --messages. (f.ex nick changes and joins etc)
        let messages = filter isMessageOrDayChanged linesInFile

        let messagesSplit = tail $ splitWhen (isDayChange) $ messages

        --print $ head messagesSplit

        

        let daysMessagesTuple = zip days messagesSplit

        --print $ daysMessagesTuple

        --let penis = fst $ head daysMessagesTuple
        
        let daysTimesMessagesTriple = map addTimeOfDay daysMessagesTuple
        
        let daysTimesNicksMessagesQuad = map addNick daysTimesMessagesTriple

        print (length (secondq (daysTimesNicksMessagesQuad !! 0)))

        mapM_ postDay daysTimesNicksMessagesQuad

        return ()

addNick :: (a,[String],[String]) -> (a,[String],[String],[String])
addNick triple =
    let nicks = map getNick (third triple)
        messages = map cleanMessages (third triple)
    in (first triple, second triple, nicks, messages)

getNick :: String -> String
getNick msg = takeWhile (/='>') (drop 8 msg)


cleanMessages :: String -> String
--don't change lines that are not messages
cleanMessages ('-':'-':'-':xs) = "---" ++ xs
--drop everything before >, and then drop the > and the space after it
cleanMessages msg = drop 2 (dropWhile (/='>') (msg))

postDay :: (UTCTime, [String], [String], [String]) -> IO () 
postDay quad = 
    let index = (length (secondq quad)) - 1
    in postToDB quad index

postToDB :: (UTCTime, [String], [String], [String]) -> Int -> IO ()
postToDB quad (-1) = print ("-1")
postToDB quad 0 = doCurl (firstq quad) ((secondq quad) !! 0) ((thirdq quad) !! 0) ((fourthq quad) !! 0)
postToDB quad idx = do
    doCurl (firstq quad) ((secondq quad) !! idx) ((thirdq quad) !! idx) ((fourthq quad) !! idx)
    print idx
    postToDB quad (idx - 1)

    

--doCurl :: a -> String -> String -> String -> ()
doCurl date time nick message =
    let curlText = curlFormat date time nick message
        address = "http://localhost:3000/lines/add"
    in curlPost address [curlText]

curlFormat :: UTCTime -> String -> String -> String -> String 
curlFormat date time nick message = 
    let timeFormat = ("%Y-%m-%dT" ++ time ++ "Z")
        timeString = (formatTime defaultTimeLocale timeFormat date)
    in "{\"message\":\"" ++ message  ++ "\", \"nick\":\"" ++ nick ++ "\", \"time\":\"" ++ timeString ++ "\"}"

--addTimeOfDay :: [(UTCTime, [String])] -> [(UTCTime, [String], [String])]
addTimeOfDay :: (a,[String]) -> (a,[String],[String])
addTimeOfDay tuple =
    let times = map getTime (snd tuple)
    in (fst tuple, times, snd tuple)


first :: (a, b, c) -> a  
first (x, _, _) = x  

second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  


firstq :: (a, b, c, d) -> a  
firstq  (x, _, _, _) = x  
 
secondq :: (a, b, c, d) -> b  
secondq (_, y, _, _) = y  
  
thirdq :: (a, b, c, d) -> c  
thirdq (_, _, z, _) = z  

fourthq :: (a, b, c, d) -> d  
fourthq (_, _, _, w) = w  




getTime :: String -> String
getTime (x:y:z:w:q:e:'<':_) = (x:y:':':z:w:':':q:e:"")
getTime _ = "aids"

isMessageOrDayChanged :: String -> Bool
isMessageOrDayChanged (x:y:z:w:q:e:'<':_) = True
isMessageOrDayChanged x = isDayChange x
--isMessageOrDayChanged ('-':'-':'-':' ':'D':'a':'y':_) = True
--isMessageOrDayChanged ('-':'-':'-':' ':'L':'o':'g':' ':'o':'p':__)= True
--isMessageOrDayChanged _ = False

isDayChange :: String -> Bool
isDayChange str = isPrefixOf "--- Day" str || isPrefixOf "--- Log opened" str
        
parseDays days = map parseDay days 

parseDay day
    | isPrefixOf "--- Day" day = parseDayChanged day
    | isPrefixOf "--- Log opened" day = parseLogOpened day

parseDayChanged dayChanged =
    let splitDayChanged = splitPlaces [11,50] $ drop 16 dayChanged
        dayChangedUTC = (head splitDayChanged) ++ "00:00:00 UTC " ++ (last splitDayChanged)
    in readTime defaultTimeLocale "%c" dayChangedUTC :: UTCTime

parseLogOpened logOpened = readTime defaultTimeLocale "%c" (drop 15 logOpened) :: UTCTime
