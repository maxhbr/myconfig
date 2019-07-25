{-# LANGUAGE LambdaCase #-}
module MyPhoto.MyPhoto
  where

import           Data.Text (Text)
import qualified Data.Text as Text

type Img = FilePath

type PActionBody = IO (Either String [Img])
data PAction = PAction ([Img] -> PActionBody)
instance Semigroup PAction where
  (PAction f1) <> (PAction f2) = PAction $
    \imgs1 ->  f1 imgs1 >>= \case
      Right imgs2 -> f2 imgs2
      err         -> pure err
instance Monoid PAction where
  mempty = PAction (\imgs -> pure (Right imgs))
runPAction :: PAction -> [Img] -> PActionBody
runPAction (PAction f) = f

type PrePAction = [String] -> PAction

logStr :: String -> PAction
logStr msg = PAction $
  \imgs -> do
    putStrLn msg
    return (Right imgs)
logSeparator :: String -> PAction
logSeparator msg = logStr ("\n################################################################################\n## " ++ msg)

  -- data PrePAction = PrePAction ([[String]] -> (PAction, [[String]]))
-- instance Semigroup PrePAction where
--   (PrePAction f1) <> (PrePAction f2) = PrePAction $
--     \args -> case f2 args of
--       (pfun1, remainingArgs) -> case f2 remainingArgs of
--         (pfun2, remainingRemainingArgs) -> (pfun1 <> pfun2, remainingRemainingArgs)
-- instance Monoid PrePAction where
--   mempty = PrePAction (\args -> (mempty, args))
