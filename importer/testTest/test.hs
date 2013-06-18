import System.Environment
import Data.List
import Data.List.Split

import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Data.Time.Clock

main = do
        [s] <- getArgs
        fileContents <- readFile s
        print s
        
        let linesInFile = lines fileContents

        let days = parseDays $ filter (isPrefixOf "---") $ linesInFile
        --print days


        let messages = tail $ splitWhen (isPrefixOf "---") $ linesInFile 


        --print $ head messages

        let juttuja = zip days messages

        --print $ head juttuja

        return ()
        
parseDays days = map parseDay days
    where parseDay str = 








        

