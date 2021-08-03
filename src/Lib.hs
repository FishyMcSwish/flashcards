{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Yesod
import Database.Persist.Sqlite
import Data.Text

share [mkPersist sqlSettings, mkMigrate "migrateAll"][persistLowerCase|
Card
  question Text
  UniqueQuestion question
  answer Text
  deriving Show
|]

mkYesod "FlashCards" [parseRoutes|
/flashcard/#CardId FlashCardR GET POST
|]


instance Yesod FlashCards where
  shouldLogIO (FlashCards _) _ _ = 
    pure True

data FlashCards = FlashCards ConnectionPool

getFlashCardR :: CardId -> HandlerFor FlashCards Value
getFlashCardR cardId = do
  $logDebug "FlashCardR GET"
  card <- runDB $ get404 cardId
  returnJson $ card

postFlashCardR :: CardId -> HandlerFor FlashCards Value
postFlashCardR cardId = do
  $logDebug "FlashCardR POST"
  $logDebug (pack $ show cardId)
  runDB $ insert_ $ Card (pack $ "q" ++ show cardId) ( pack $ "a" ++ show cardId)
  card <- runDB $ get404 cardId
  returnJson $ card



instance YesodPersist FlashCards where
  type YesodPersistBackend FlashCards = SqlBackend

  runDB action = do 
    FlashCards pool <- getYesod
    runSqlPool action pool

instance ToJSON Card where
  toJSON Card {..} = object
    ["question" .= cardQuestion
    , "answer" .= cardAnswer
    ]
