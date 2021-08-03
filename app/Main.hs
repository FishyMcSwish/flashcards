{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Yesod
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

main :: IO ()
main = 
  runStderrLoggingT $ withSqlitePool "test.db3" 5 $ \pool -> liftIO $ do
    runResourceT $ flip runSqlPool pool $ do
        runMigration migrateAll
        card <- getBy $ UniqueQuestion "question1"
        case card of
          Nothing ->
            do
              cardId <- insert $ Card "question1" "answer1"
              pure ()
          Just card ->
            pure ()
    warp 3000 $ FlashCards pool


