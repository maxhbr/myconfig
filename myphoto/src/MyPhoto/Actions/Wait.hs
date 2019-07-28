module MyPhoto.Actions.Wait
    ( waitPAct
    ) where

import           System.Process
import           System.IO
import           Control.Concurrent

import MyPhoto.Model
import MyPhoto.Utils

waitForConfirm :: [Img] -> PActionBody
waitForConfirm imgs = do
  x <- getChar
  return (Right imgs)

waitForSeconds :: String -> [Img] -> PActionBody
waitForSeconds s imgs = do
  let ms = 1000000 * (read s :: Int)
  threadDelay ms
  return (Right imgs)


help :: String
help = "wait [-s SECONDS]"

waitPAct :: PrePAction
waitPAct ["-h"] = PAction (\_ -> pure (Left help))
waitPAct [] = logSeparator "Wait for key" <> PAction waitForConfirm
waitPAct ["-s", s] = logSeparator ("Wait for " ++ s ++ " seconds") <> PAction (waitForSeconds s)
