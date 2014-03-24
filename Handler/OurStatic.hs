module Handler.OurStatic
  ( getOurStaticR       -- ノムニチトップ
  )
where

import Import
import Data.Time
import System.Locale (defaultTimeLocale)
import System.IO.Unsafe (unsafePerformIO) 

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

getOurStaticR :: String -> Handler Html
getOurStaticR top =  defaultLayout $ do
    setTitle "Test"
    $(widgetFile "ourstatic")

