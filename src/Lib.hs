{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Lib where

import Yesod

instance Yesod FlashCards where
  shouldLogIO FlashCards _ _ = 
    pure True

data FlashCards = FlashCards

mkYesod "FlashCards" [parseRoutes|
/flashcard/#Int FlashCardR GET
|]


getFlashCardR :: Int -> HandlerFor FlashCards Value
getFlashCardR number = do
  $logDebug "FlashCardR GET"
  pure $ object ["msg" .= ("you got flashcard number " ++ (show number) :: String)]

