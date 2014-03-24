module Handler.OurStatic
  ( getOurStaticR       
  )
where

import Import
import Data.Time
import Data.List
import System.Locale (defaultTimeLocale)
import System.IO.Unsafe (unsafePerformIO) 
import System.IO (readFile)
import Text.Blaze.Html (preEscapedToHtml)

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

readStaticFile :: FilePath -> Html
readStaticFile filePath = preEscapedToHtml $ unsafePerformIO $ readFile filePath

getOurStaticR :: String -> Handler Html
getOurStaticR path = do
    defaultLayout $ do
      [whamlet|#{readStaticFile ("public_html/" ++ path)}|]

