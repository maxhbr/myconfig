module MyPhoto.Actions.Wait
    ( waitPAct
    ) where

import           System.Process
import           System.IO
import           Control.Concurrent

import MyPhoto.MyPhoto

waitForConfirm :: [Img] -> PActionBody
waitForConfirm imgs = do
  x <- getChar
  return (Right imgs)

waitForSeconds :: String -> [Img] -> PActionBody
waitForSeconds s imgs = do
  let ms = 1000000 * (read s :: Int)
  threadDelay ms
  return (Right imgs)

waitPAct :: PrePAction
waitPAct ["-h"] = undefined
waitPAct [] = logSeparator "Wait for key" <> PAction waitForConfirm
waitPAct ["-s", s] = logSeparator ("Wait for " ++ s ++ " seconds") <> PAction (waitForSeconds s)
