{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}

module Handler.Lines where

import Import
import Network.HTTP.Types (status200)




getLinesR :: Handler RepHtml
getLinesR = do 
    values :: [Entity Line] <- (runDB $ selectList [] []) 
    defaultLayout $(widgetFile "lines")

