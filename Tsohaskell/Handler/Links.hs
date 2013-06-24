{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Links where

import Import
import Database.Esqueleto
import Data.Int (Int64)
import qualified Yesod.Links as L

getLinksR :: Handler Html
getLinksR = defaultLayout $ linksWidget 10

linksWidget :: Int64 -> Widget
linksWidget num = do
    linkEntities :: [(Entity Link, Entity Line)] <- handlerToWidget $ runDB $ 
        select $
        from $ \(link, line) -> do
            where_ (link ^. LinkLine Database.Esqueleto.==. line ^. LineId)
            orderBy [desc (line ^. LineTime)]
            limit num
            return (link, line)
    let links = map (\(link, line) -> (entityVal link, entityVal line)) linkEntities
    $(widgetFile "links")
        where
            mkLink t = L.link' $ L.Link (L.External t) t t
