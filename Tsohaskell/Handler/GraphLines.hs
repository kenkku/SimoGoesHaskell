{-# LANGUAGE ScopedTypeVariables #-}
module Handler.GraphLines where

import Import
import Database.Persist.Sql
import Graphics.GChart
import Data.List
import Data.Maybe

getGraphLinesR :: Handler Html
getGraphLinesR = defaultLayout graphLinesWidget

graphLinesWidget :: Widget
graphLinesWidget = do
    values :: [(Single String, Single String, Single Int)] <- handlerToWidget messages
    let dates = nub $ map unSingle $ first $ unzip3 values
    let nicks = nub $ map unSingle $ second $ unzip3 values
    let countsMax = maximum $ map unSingle $ third $ unzip3 values
    let countsMaxFloat = fromIntegral countsMax :: Float
    let nicksLines = createTuple (length dates) nicks
    let dbResults = map unSingleValues values

    let chartData = addActualLineCounts nicksLines dbResults dates

    let graph = linexyGraph2 chartData countsMaxFloat dates


    $(widgetFile "graphlines")
    where
        messages = runDB $ rawSql "SELECT date(substr(time, 0, length(time) - 3)) as day, nick, count(*) as linecount FROM line WHERE time > (SELECT DATETIME('now', '-14 day')) GROUP BY nick, day ORDER BY day ASC, linecount DESC" []

each :: Int -> [a] -> [a]
each n = map head . takeWhile (not . null) . iterate (drop n)


linexyGraph2 chartData countsMaxFloat dates = 
    getChartUrl $ do setChartSize 800 300
                     setChartType Graphics.GChart.Line
                     setDataEncoding text
                     setChartTitle "Lines per day"
                        
                     let penis = unzip chartData

                     --let datesFifth = each 5 dates

                     let days = map (reverse . (take 2) . reverse) dates

                     --let nicks = fst penis
                     let nicks = take 5 $ fst penis

                     let sinep = map (map fromIntegral) (snd penis) :: [[Float]]

                     let multiplier = 100 / countsMaxFloat

                     --let scaledData = map (map (* multiplier)) sinep
                     let scaledData = take 5 $ map (map (* multiplier)) sinep
                
                     setGrid $ makeGrid { xAxisStep = 3.333,
                                            yAxisStep = 10,
                                            lineSegmentLength = Just 1,
                                            blankSegmentLength = Just 3 }
                     --

                     addAxis $ makeAxis { axisType = AxisLeft,
                                          
                                         axisRange = Just $ Range (0, countsMaxFloat) (Just 100) }

                     addAxis $ makeAxis { axisType = AxisBottom,
                                         axisLabels = Just $ days}

                     
                     --addChartData $ head scaledData
                     mapM_ addChartData scaledData
                    
                    {--
                     setColors ["FF0000",
                                "0000FF",
                                "009933",
                                "000066",
                                "FFCC00" ]
                        --}
                     setColors ["006699",
                                "0000FF",
                                "CC00FF",
                                "FF66CC",
                                "FF6600",
                                "CCFF33",
                                "009900",
                                "669999",
                                "663300"]

                     setLegend $ legendWithPosition nicks LegendRight

createTuple :: Int -> [String] -> [(String, [Int])]
createTuple dayCount nicks = zip nicks $ replicate (length nicks) $ replicate dayCount 0



addActualLineCounts :: [(String, [Int])] -> [(String, String, Int)] -> [String] -> [(String, [Int])]
addActualLineCounts nicksLines dbResults dates =
    let index = length dbResults
    in addCountsRecursive nicksLines dbResults dates (index - 1)

addCountsRecursive :: [(String, [Int])] -> [(String, String, Int)] -> [String] -> Int -> [(String, [Int])]
addCountsRecursive nicksLines dbResults dates 0 = 
    let nick = second $ dbResults !! 0 
        dayIdx = fromMaybe (-1) $ elemIndex (first $ dbResults !! 0) dates
        lineCount = third $ dbResults !! 0
        nickCount = length nicksLines
    in findAndAddRecursive nick dayIdx lineCount nicksLines (nickCount - 1)        
addCountsRecursive nicksLines dbResults dates dbResultsIndex =
    let nick = second $ dbResults !! dbResultsIndex
        dayIdx = fromMaybe (-1) $ elemIndex (first $ dbResults !! dbResultsIndex) dates
        lineCount = third $ dbResults !! dbResultsIndex
        nickCount = length nicksLines
        newNicksLines = findAndAddRecursive nick dayIdx lineCount nicksLines (nickCount - 1)
    in addCountsRecursive newNicksLines dbResults dates (dbResultsIndex - 1)

findAndAddRecursive :: String -> Int -> Int -> [(String, [Int])] -> Int -> [(String, [Int])]
findAndAddRecursive nick dayIdx lineCount nicksLines 0
    -- | fst (nicksLines !! 0) /= nick = nicksLines
    | fst (nicksLines !! 0) /= nick = error ("nick: '" ++ nick ++ "' when wanted: " ++ (fst (nicksLines !! 0)))
    | otherwise = modifyList nicksLines 0 (nick, modifyList (snd $ nicksLines !! 0) dayIdx lineCount)
findAndAddRecursive nick dayindex lineCount nicksLines index
    | fst (nicksLines !! index) == nick = modifyList nicksLines index (nick, modifyList (snd $ nicksLines !! index) dayindex lineCount)
    | otherwise = findAndAddRecursive nick dayindex lineCount nicksLines (index -1) 


--Oh god y I do dis??
modifyList :: [a] -> Int -> a -> [a]
modifyList oldList 0 newValue = 
    newValue:(tail oldList)
modifyList oldList indexToChange newValue = 
    let (xs, ys) = splitAt (indexToChange) oldList
    in xs++(newValue:(tail ys))
    

unSingleValues :: (Single a, Single b, Single c) -> (a, b, c)
unSingleValues values = ((unSingle $ first values), (unSingle $ second values), (unSingle $ third values))


first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

