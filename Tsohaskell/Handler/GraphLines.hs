module Handler.GraphLines where

import Import
import Database.Persist.Sql

getGraphLinesR :: Handler Html
getGraphLinesR = do
    values <- messages
    defaultLayout $(widgetFile "linesgraph")
    where
        messages :: Handler [(Single String, Single String, Single String)]
        --messages :: Handler [(Single String, Single String)]
        messages = runDB $ rawSql "SELECT nick, strftime('%d', substr(time, 0, length(time) - 3)), count(*) as linecount FROM line GROUP BY nick, strftime('%d', substr(time, 0, length(time) - 3)) ORDER BY strftime('%d', substr(time, 0, length(time) - 3)) ASC, linecount DESC" []
        --messages = runDB $ rawSql "SELECT nick, time, COUNT(nick) FROM line GROUP BY strftime('%H', substr(time, 0, length(time) - 3))" []
        --messages = runDB $ rawSql "" []
        --messages = runDB $ rawSql "SELECT nick, message, strftime('%d', substr(time, 0, length(time) - 3)) as herpderp FROM line ORDER BY time" []
        --messages = runDB $ rawSql "SELECT nick, message, substr(time, 0, length(time) - 3) as herpderp FROM line ORDER BY time" []


--strftime ('%d', time) as day 

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

