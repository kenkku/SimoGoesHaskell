{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Lines where

import Import
import Database.Persist.Sql (Single, rawSql, unSingle)

getLinesR :: Handler Html
getLinesR = defaultLayout linesWidget

linesWidget :: Widget
linesWidget = do
	values :: [(Single String, Single Int)] <- handlerToWidget selectLinecounts
	$(widgetFile "lines")
  where
  	selectLinecounts = runDB $ rawSql "SELECT nick, COUNT(*) FROM line GROUP BY nick ORDER BY 2 DESC" []	
