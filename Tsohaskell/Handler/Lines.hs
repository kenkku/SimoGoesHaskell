{-# LANGUAGE ScopedTypeVariables, QuasiQuotes #-}

module Handler.Lines where

import Import
import Network.HTTP.Types (status200)




getLinesR :: Handler RepHtml
getLinesR = do 
    values :: [Entity Line] <- (runDB $ selectList [] []) 
    defaultLayout [whamlet|
<ul>
    $forall line <- values
        <li>#{lineNick (entityVal line)} #{lineMessage (entityVal line)}
|] 

--   liftIO $ print (values :: [Entity Line])
