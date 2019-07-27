{-# LANGUAGE LambdaCase #-}
module MyPhoto.Utils
  where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad ((>=>))

import MyPhoto.Model

logStr :: String -> PAction
logStr msg = PAction $
  \imgs -> do
    putStrLn msg
    return (Right imgs)

line :: String
line = "################################################################################"
endLine :: PAction
endLine = PAction $
  \imgs -> do
    putStrLn ("## (#=" ++ show (length imgs) ++ ")")
    putStrLn line
    return (Right imgs)
logSeparator :: String -> PAction
logSeparator "" = logStr line
logSeparator msg = logStr ( line ++"\n## " ++ msg)

filterByExtension :: [String] -> PAction
filterByExtension = undefined

assertThatAllExist :: PAction
assertThatAllExist = undefined
