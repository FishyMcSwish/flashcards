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
{-# LANGUAGE ScopedTypeVariables #-}

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
/flashcard/#CardId FlashCardR GET
/flashcard FlashCardCreateR POST
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

postFlashCardCreateR :: HandlerFor FlashCards Value
postFlashCardCreateR  = do
  $logDebug "FlashCardR POST"
  (cardReq :: Card) <- requireCheckJsonBody
  cardId <- runDB $ insert $ cardReq
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

instance FromJSON Card where
  parseJSON (Object v) = Card <$> v.: "question" <*> v.: "answer"
  parseJSON _ = undefined
