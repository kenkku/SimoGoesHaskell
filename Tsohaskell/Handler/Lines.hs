{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Lines where

import Import
import Database.Persist.Sql (Single, rawSql, unSingle)



getLinesR :: Handler Html
getLinesR = do 
    values <- selectLinecounts
    defaultLayout $(widgetFile "lines")
  where
  	selectLinecounts :: Handler [(Single String, Single Int)]
  	selectLinecounts = runDB $ rawSql "SELECT nick, COUNT(*) FROM line GROUP BY nick ORDER BY 2 DESC" []

