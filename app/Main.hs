module Main where

import Lib
import Yesod

main :: IO ()
main = warp 3000 FlashCards
