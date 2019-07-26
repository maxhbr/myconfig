{-# LANGUAGE LambdaCase #-}
module MyPhoto.MyPhoto
  where

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad ((>=>))

type Img = FilePath

type PActionBody = IO (Either String [Img])
newtype PAction = PAction ([Img] -> PActionBody)
instance Semigroup PAction where
  (PAction f1) <> (PAction f2) = PAction $
    f1 >=> \case
      Right imgs2 -> f2 imgs2
      err         -> pure err
instance Monoid PAction where
  mempty = PAction (pure . Right)
runPAction :: PAction -> [Img] -> PActionBody
runPAction (PAction f) = f

type PrePAction = [String] -> PAction

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
