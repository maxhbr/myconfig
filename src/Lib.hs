{-# LANGUAGE LambdaCase #-}
module Lib
    ( module X
    , runMyPhoto
    ) where

import           System.Environment
import           Data.Map (Map)
import           Control.Monad
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import MyPhoto.MyPhoto
import MyPhoto.Actions.UnRAW as X
import MyPhoto.Actions.Align as X
import MyPhoto.Actions.Stack as X

actions :: Map String PrePAction
actions = Map.fromList [("unraw", unRAW), ("align", align), ("stack", stack)]

type ComposeActionsState = (PAction, [String], Maybe PrePAction)
composeActions :: [String] -> (PAction, [Img])
composeActions = let

    composeActions' :: ComposeActionsState -> String -> ComposeActionsState
    composeActions' (act, opts, Nothing) opt = case opt `Map.lookup` actions of
      Just preAct -> (act, [], Just preAct) -- TODO: assert that otps were empty
      Nothing     -> (act, opts ++ [opt], Nothing)
    composeActions' (act, opts, Just preAct) "--" = (act <> preAct opts, [], Nothing)
    composeActions' (act, opts, Just preAct) opt  = case opt `Map.lookup` actions of
      Just preAct2 -> (act <> preAct opts, [], Just preAct2)
      Nothing      -> (act, opts ++ [opt], Just preAct)

  in (\case
         (act, imgs, Nothing) -> (act, imgs)
         (act, opts, Just preAct) -> (act <> preAct opts, []))
     . foldl composeActions' (mempty, [], Nothing)

runMyPhoto :: IO ()
runMyPhoto = do
  args <- getArgs
  let (act, imgs) = composeActions args
  result <- runPAction act imgs
  case result of
    Left err   -> putStrLn err
    Right imgs -> mapM_ putStrLn imgs
