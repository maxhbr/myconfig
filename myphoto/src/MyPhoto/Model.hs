{-# LANGUAGE LambdaCase #-}
module MyPhoto.Model
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
