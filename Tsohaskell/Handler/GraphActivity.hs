{-# LANGUAGE ScopedTypeVariables #-}
module Handler.GraphActivity where

import Import
import Data.Time.Calendar
import Data.Time.Clock
import Database.Persist.Sql (rawSql, Single, unSingle)

getGraphActivityR :: Handler Html
getGraphActivityR = defaultLayout graphActivityWidget

graphActivityWidget :: Widget
graphActivityWidget = do
    linecounts :: [(Single String, Single Int)] <- handlerToWidget selectLinecounts
    $(widgetFile "activitygraph")
  where
    selectLinecounts = runDB $ rawSql "SELECT strftime('%d.%m.', substr(time, 0, length(time) - 3)) as day, count(*) as linecount FROM line GROUP BY day ORDER BY day DESC LIMIT 7" []

            