module Handler.LineAdd where

import Import
import Data.Aeson.TH
import Data.Time

data Message = Message 
			{ message :: Text
			, nick :: Text
			, time :: UTCTime
			}

$(deriveFromJSON id ''Message)

postLineAddR :: Handler ()
postLineAddR = do 
	value <- parseJsonBody_
	runDB $ insert_ $ Line (message value) (nick value) (time value)

