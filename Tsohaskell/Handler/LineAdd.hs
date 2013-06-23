{-# LANGUAGE ScopedTypeVariables #-}
module Handler.LineAdd where

import Import
import Data.Aeson.TH (deriveFromJSON)
import Data.Aeson.Types
import Data.Char (toLower)
import Data.Text (unpack, pack)
import Text.Regex.TDFA

$(deriveFromJSON (drop 4 . map toLower) ''Line)

postLineAddR :: Handler RepPlain
postLineAddR = do 
	value <- parseJsonBody
	_ <- case value of
		Error s -> sendResponse s
		Success (val :: Line) -> do
                    runDB $ do
                        newline <- insert val
                        case matchLink (lineMessage val) of
                            Just link -> insert_ $ Link (pack link) newline
                            Nothing -> return ()
	sendResponse () 

matchLink :: Text -> Maybe String
matchLink t = (unpack t) =~~ ("https*://[^ ]+" :: String)
