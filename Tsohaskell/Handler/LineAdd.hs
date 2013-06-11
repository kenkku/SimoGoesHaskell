module Handler.LineAdd where

import Import
import Data.Aeson.TH (deriveFromJSON)
import Data.Aeson.Types
import Data.Char (toLower)

$(deriveFromJSON (drop 4 . map toLower) ''Line)

postLineAddR :: Handler RepPlain
postLineAddR = do 
	value <- parseJsonBody
	_ <- case value of
		Error s -> sendResponse s
		Success val -> runDB $ insert (val :: Line)
	sendResponse ()
	