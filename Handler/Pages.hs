module Handler.Pages 
    ( getPagesR       -- pagesのトップ
    , postPagesR      -- pagesの投稿
    , getWebpageR     -- pagesの表示
    , postWebpageR    -- pagesの編集
    , getEditWebpageR -- pagesの編集画面の表示
    )
where

import Import
import Data.Monoid
import Data.Time
import System.Locale (defaultTimeLocale)
import Settings 
import Data.Maybe

import Yesod.Form.Nic (YesodNic, nicHtmlField)
instance YesodNic App

entryForm :: Form Webpage
entryForm = renderDivs $ Webpage
  <$> areq textField    "Title"        Nothing
  <*> areq nicHtmlField "Content"      Nothing
  <*> areq textField    "PermaLink"    Nothing
  <*> areq textField    "Author"       Nothing
  <*> aformM (liftIO getCurrentTime)
  <*> aformM (liftIO getCurrentTime)

editForm :: Maybe Webpage -> Form Webpage
editForm webpage = renderDivs $ Webpage
  <$> areq textField    "Title"        (webpageTitle <$> webpage)
  <*> areq nicHtmlField "Content"      (webpageContent <$> webpage)
  <*> areq textField    "PermaLink"    (webpagePermaLink <$> webpage)
  <*> areq textField    "Author"       (webpageAuthor <$> webpage)
  <*> aformM (liftIO getCurrentTime)   
  <*> aformM (liftIO getCurrentTime)

getPagesR :: Handler RepHtml
getPagesR = do
  -- Get the list of webpages inside the database
  webpages <- runDB $ selectList [][Desc WebpageTitle]
  -- We'll need the two "objects": webpageWidget and enctype
  -- to construct the form (see templates/webpages.hamlet).
  (webpageWidget, enctype) <- generateFormPost entryForm
  defaultLayout $ do
    $(widgetFile "webpages")

postPagesR :: Handler RepHtml
postPagesR = do
  ((res,webpageWidget),enctype) <- runFormPost entryForm
  case res of
     FormSuccess webpage -> do
       webpageId <- runDB $ insert webpage
       setMessage $ toHtml $ (webpageTitle webpage) <> " created"
       redirect $ WebpageR webpageId
     _ -> defaultLayout $ do
       setTitle "Please correct your entry form"
       $(widgetFile "webpageAddError")

getWebpageR :: WebpageId -> Handler RepHtml
getWebpageR webpageId = do
    webpage <- runDB $ get404 webpageId
    defaultLayout $ do
        setTitle $ toHtml $ webpageTitle webpage
        $(widgetFile "webpage")

-- 編集画面
getEditWebpageR :: WebpageId -> Handler RepHtml
getEditWebpageR webpageId = do
  webpage <- runDB $ get404 webpageId
  (webpageWidget, enctype) <- generateFormPost $ editForm (Just webpage)
  defaultLayout $ do
    $(widgetFile "editWebpageForm")

-- 記事更新
postWebpageR :: WebpageId -> Handler RepHtml
postWebpageR webpageId = do
  ((res, webpageWidget), enctype) <- runFormPost (editForm Nothing)
  case res of
    FormSuccess webpage -> do
      runDB $ do
        _webpage <- get404 webpageId
        update webpageId
          [ WebpageTitle   =. webpageTitle   webpage
          , WebpageContent =. webpageContent webpage
          , WebpagePermaLink =. webpagePermaLink webpage
          , WebpageAuthor =. webpageAuthor webpage
          , WebpageUpdatedOn =. webpageUpdatedOn webpage
          ]
      setMessage $ toHtml $ (webpageTitle webpage) <> " is updated."
      redirect $ WebpageR webpageId
    _ -> defaultLayout $ do
      setTitle "Please correct your entry form."
      $(widgetFile "editWebpageForm")